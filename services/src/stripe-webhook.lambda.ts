import { APIGatewayEvent, APIGatewayProxyResult } from "aws-lambda";
import { Stripe } from "stripe";
import { DynamoDB } from "aws-sdk";
import { getStripeApiKey } from "./utils/config";
import * as AWS from "aws-sdk";
import * as dotenv from "dotenv";
import { handlePayoutPaid } from "./webhook/payout-paid/payout-paid.handler";
import { successResponse } from "./utils/success-response";
import { handleReportRunSucceeded } from "./webhook/run-report-succeeded/report-run-succeeded.handler";

dotenv.config();

const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const committeeTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const runenv: any = process.env.RUNENV;
let stripe: Stripe;
let stripeApiKey: string;
let dynamoDB: DynamoDB;

const ps = new AWS.SSM();

export default async (
  event: APIGatewayEvent
): Promise<APIGatewayProxyResult> => {
  console.log("Event: ", event.body);
  if (!stripeApiKey || !stripe) {
    console.log("Setting up configuration");
    stripeApiKey = await getStripeApiKey(ps)(runenv);
    stripe = new Stripe(stripeApiKey, {
      apiVersion: "2020-08-27",
    });
    console.log("Configuration values have been set");
  }

  const payload = JSON.parse(event.body);

  if (!payload?.livemode) return successResponse;

  switch (payload?.type) {
    case "reporting.report_run.succeeded":
      console.log("reporting.report_type.succeeded event happened");
      if (
        payload?.data?.object?.report_type ===
        "connected_account_payout_reconciliation.itemized.5"
      ) {
        return await handleReportRunSucceeded(txnsTableName)(
          committeeTableName
        )(dynamoDB)(stripeApiKey)(payload);
      } else {
        return successResponse;
      }
    case "payout.paid":
      return await handlePayoutPaid(stripe)(payload);
    default:
      console.log(`Unhandled event type ${payload.type}`);
      return successResponse;
  }
};
