import { expect } from "chai";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { ILexisNexisConfig } from "../../src/clients/lexis-nexis/lexis-nexis.client";
import { genCommittee } from "../utils/gen-committee.util";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import { ActBlueCSVType } from "../../src/clients/actblue/actblue.decoders";
import { Stripe } from "stripe";
import { deleteCommittee } from "../../src/utils/model/committee/delete-committee.utils";
import { getStripeApiKey } from "../../src/utils/config";
import { putCommittee } from "../../src/utils/model/committee/put-committee.utils";
import { putTransaction } from "../../src/utils/model/transaction/put-transaction.utils";
import { toMockContrib } from "../../src/demo/utils/seed-bank-records.util";
import { committeeToAC } from "../utils/committee-to-actblue-sync.utils";
import { ITransaction } from "../../src/model/transaction.type";
import { ReconcileTxnInput } from "../../src/graphql/input-types/reconcile-txn.input-type";
import { lambdaPromise } from "../../src/utils/lambda-promise.util";
import graphql from "../../src/committee-graphql.lambda";
import { genGraphQLProxy } from "../utils/gen-allowed-proxy.util";
import { getTxnQuery, recTxnMutation } from "../utils/graphql.utils";
import { qaUsers } from "../seed/qa-users.data";
import {
  ISyncContribResult,
  Result,
} from "../../src/pipes/external-contribs/external-txns-to-ddb.pipe";
import { groupContribByPayoutId } from "../utils/group-contribs-by-payout-id.util";
import { calcExtPayoutSet } from "../utils/calc-ext-payout-sum.util";

dotenv.config();

const billableEventsTable: any = process.env.BILLABLE_EVENTS_DDB_TABLE_NAME;
const transactionsTable: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const committeesTable: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const donorsTable: any = process.env.DONORS_DDB_TABLE_NAME;
const rulesTable: any = process.env.RULES_DDB_TABLE_NAME;
const runenv: any = process.env.RUNENV;

const lnUsername = process.env.LN_USERNAME;
const lnPassword = process.env.LN_PASSWORD;
const actBlueSecret = process.env.ACTBLUE_CLIENT_SECRET;
const actBlueUuid = process.env.ACTBLUE_CLIENT_UUID;
const actBlueAccountId = process.env.ACTBLUE_ACCOUNT_ID;
const ps = new AWS.SSM();
let stripe: Stripe;

const lnConfig: ILexisNexisConfig = {
  username: lnUsername,
  password: lnPassword,
};

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const ddb = new DynamoDB();

const committee = genCommittee({
  id: `rec-txns-${genTxnId()}`,
  district: "53",
  officeType: "senate",
  party: "democrat",
  race: "primary",
  ruleVersion: "nyboe-2020",
  scope: "state",
  state: "ny",
  tzDatabaseName: "America/New_York",
  actBlueAccountId,
  actBlueAPICredentials: {
    clientUUID: actBlueUuid,
    clientSecret: actBlueSecret,
  },
});

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const reportType = ActBlueCSVType.PaidContributions;

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const validUsername = qaUsers[0];

let payoutSetIds: string[];
let wrongPayoutSetIds: string[];
let payoutSetSum: number;
let wrongPayoutSetSum: number;
let matchingBankRecord: ITransaction;
let unmatchingBankRecord: ITransaction;
describe("Transaction Reconciliation", function () {
  before(async () => {
    const stripeApiKey = await getStripeApiKey(ps)("qa");
    stripe = new Stripe(stripeApiKey, {
      apiVersion: "2020-08-27",
    });

    await putCommittee(committeesTable)(ddb)(committee);

    // import ActBlue transactions
    const acContribs = await committeeToAC({
      committee,
      lnConfig,
      transactionsTable,
      billableEventsTable,
      rulesTable,
      donorsTable,
      committeesTable,
      dynamoDB: ddb,
      stripe,
    });

    // Valid set
    const payoutSets = groupContribByPayoutId(acContribs);
    const payoutSetId = Object.keys(payoutSets)[0];
    const payoutSet = payoutSets[payoutSetId];
    payoutSetIds = payoutSet.map((txn) => txn.id);
    payoutSetSum = calcExtPayoutSet(payoutSet);

    // Invalid set
    const wrongPayoutSetId = Object.keys(payoutSets)[1];
    const wrongPayoutSet = payoutSets[wrongPayoutSetId];
    wrongPayoutSetIds = wrongPayoutSet.map((txn) => txn.id);
    wrongPayoutSetSum = calcExtPayoutSet(wrongPayoutSet);

    // generate bank record transaction with matching total
    matchingBankRecord = await putTransaction(transactionsTable)(ddb)(
      toMockContrib(payoutSetSum)(committee.id)
    );

    // generate bank record with non-matching total
    unmatchingBankRecord = await putTransaction(transactionsTable)(ddb)(
      toMockContrib(50)(committee.id)
    );
    console.log("payout set id", payoutSetId);
  });
  describe("Sum of Selected external transactions do not match the bank transaction", function () {
    it("Returns error", async () => {
      const recContribVars: ReconcileTxnInput = {
        committeeId: committee.id,
        bankTransaction: matchingBankRecord.id,
        selectedTransactions: wrongPayoutSetIds,
      };

      const createRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(recTxnMutation, validUsername, recContribVars),
        {}
      );

      const resBody = JSON.parse(createRes.body);

      expect(resBody.errors[0].message).to.equal(
        `Selected transactions sum of ${wrongPayoutSetSum} must add up to bank transaction amount ${matchingBankRecord.amount}`
      );
    });
  });
  after(async () => {
    console.log("committee ID", committee.id);

    await deleteCommittee(committeesTable)(ddb)(committee);
  });
});
