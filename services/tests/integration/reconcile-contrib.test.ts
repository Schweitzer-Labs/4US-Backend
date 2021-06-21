import { getStripeApiKey } from "../../src/utils/config";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { Stripe } from "stripe";
import { runReport } from "../../src/request-report";

const stripeAccount = "acct_1IjTcsRC8iiQex3V";
AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const runenv: any = process.env.RUNENV;
const transactionsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const committeeTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
let stripe: Stripe;
let stripeApiKey: string;
let dynamoDB: DynamoDB;

describe("Reconcile contributions flow", function () {
  before(async () => {
    if (!stripeApiKey || !stripe) {
      console.log("Setting up configuration");
      stripeApiKey = await getStripeApiKey(runenv);
      stripe = new Stripe(stripeApiKey, {
        apiVersion: "2020-08-27",
      });
      console.log("Configuration values have been set");
    }
  });
  describe("Create Payout Report Run", function () {
    it("Receive a valid response from a valid request", async () => {
      const res = await runReport(stripe)(stripeAccount);
      console.log(res);
    });
  });
});
