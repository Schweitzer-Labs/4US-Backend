import { expect } from "chai";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { ILexisNexisConfig } from "../../src/clients/lexis-nexis/lexis-nexis.client";
import { DynamoDB } from "aws-sdk";
import { genCommittee } from "../utils/gen-committee.util";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import { ActBlueCSVType } from "../../src/clients/actblue/actblue.decoders";
import { IExternalContrib } from "../../src/model/external-data.type";
import { Stripe } from "stripe";
import { deleteCommittee } from "../../src/utils/model/committee/delete-committee.utils";
import { getStripeApiKey } from "../../src/utils/config";
import { putCommittee } from "../../src/utils/model/committee/put-committee.utils";

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

describe("Transaction Reconciliation", function () {
  before(async () => {
    const stripeApiKey = await getStripeApiKey(ps)("qa");
    stripe = new Stripe(stripeApiKey, {
      apiVersion: "2020-08-27",
    });

    await putCommittee(committeesTable)(ddb)(committee);
  });
  describe("External Contrib", function () {
    before(async () => {
      // sync ActBlue transactions.
      // generate bank record transaction with matching total
      // generate bank record with non-matching total
    });
    describe("Valid Inputs", function () {
      before(async () => {
        // invoke valid attempt to reconcile
      });
      it("Removes bank transaction after reconciliation", async () => {});
      it("Sets external contribs and fees to bank verified and tags them with bank data", async () => {});
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
    await deleteCommittee(committeesTable)(ddb)(committee);
  });
});

expect(true).to.equal(false);
