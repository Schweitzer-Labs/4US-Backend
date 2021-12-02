import { expect } from "chai";

import { getStripeApiKey } from "../../src/utils/config";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { Stripe } from "stripe";
import { runReport, runReportAndDecode } from "../../src/request-report";
import { isLeft } from "fp-ts/Either";
import { ApplicationError } from "../../src/utils/application-error";

const stripeAccount = "acct_1IjTcsRC8iiQex3V";
AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const dynamoDB = new DynamoDB();

const runenv: any = process.env.RUNENV;
const transactionsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const committeeTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
let stripe: Stripe;
let stripeApiKey: string;

describe("Reconcile contributions flow", function () {
  before(async () => {
    if (!stripeApiKey || !stripe) {
      stripeApiKey = await getStripeApiKey(runenv);
      stripe = new Stripe(stripeApiKey, {
        apiVersion: "2020-08-27",
      });
      console.log("Configuration values have been set");
    }
  });
  describe("Payout notification processing", function () {
    it("Sends a request to run a payout report", async () => {
      const res = await runReportAndDecode(stripe)(stripeAccount)();
      if (isLeft(res)) {
        throw new ApplicationError("Run report decode failed", {});
      }
      const status = res.right.status;
      expect(status).to.equal("pending");
    });
  });
  describe("Payout Report Updated processing", function () {
    it("Receive a valid response from a valid request", async () => {
      const res = await runReportAndDecode(stripe)(stripeAccount)();
      if (isLeft(res)) {
        throw new ApplicationError("Run report decode failed", {});
      }
      const status = res.right.status;
      expect(status).to.equal("pending");
    });
  });
});
