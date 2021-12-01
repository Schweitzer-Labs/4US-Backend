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

let acSum: number;
let acContribs: ISyncContribResult[];
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
    acContribs = await committeeToAC({
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

    acSum = acContribs.reduce((acc, val) => {
      let amount =
        val.result === Result.Created
          ? val.externalContribution.amount +
            val.externalContribution.processorFeeData.amount
          : 0;
      return amount + acc;
    }, 0);
    // generate bank record transaction with matching total
    matchingBankRecord = await putTransaction(transactionsTable)(ddb)(
      toMockContrib(acSum)(committee.id)
    );

    // generate bank record with non-matching total
    unmatchingBankRecord = await putTransaction(transactionsTable)(ddb)(
      toMockContrib(50)(committee.id)
    );
  });
  describe("External Contrib", function () {
    describe("Valid Inputs", function () {
      before(async () => {
        const recContribVars: ReconcileTxnInput = {
          committeeId: committee.id,
          bankTransaction: matchingBankRecord.id,
          selectedTransactions: acContribs.map((val) => val.transaction.id),
        };

        const res = await lambdaPromise(
          graphql,
          genGraphQLProxy(recTxnMutation, validUsername, recContribVars),
          {}
        );
        console.log("rec txn response", res);
      });
      it("Removes bank transaction after reconciliation", async () => {
        const getTxnRes: any = await lambdaPromise(
          graphql,
          genGraphQLProxy(
            getTxnQuery(committee.id)(matchingBankRecord.id),
            validUsername,
            {}
          ),
          {}
        );
        const txnResBody = JSON.parse(getTxnRes.body);

        console.log();
        expect(txnResBody.errors.length > 0).to.equal(true);
      });
      it("Sets external contribs and fees to bank verified and tags them with bank data", async () => {
        expect(true).to.equal(false);
      });
    });
    describe("Invalid Inputs", function () {
      describe("Non-existent External Payout ID entered", function () {
        before(async () => {
          // invoke invalid attempt to reconcile with bad external payout ID
        });
        it("Returns error", async () => {});
      });
      describe("Non-existent bank record transaction", function () {
        before(async () => {
          // invoke invalid attempt to reconcile with bad bank record ID
        });
        it("Returns error", async () => {});
      });
      describe("Mismatched payout to bank transaction", function () {
        before(async () => {
          // invoke invalid attempt to reconcile with
        });
        it("Returns error", async () => {});
      });
    });
  });
  after(async () => {
    console.log("committee ID", committee.id);
    await deleteCommittee(committeesTable)(ddb)(committee);
  });
});
