import { APIGatewayEvent, APIGatewayProxyResult } from "aws-lambda";
import { successResponse } from "./utils/success-response";
import {
  getReportAndDecode,
  reportEventToUrl,
  runReport,
} from "./request-report";
import { pipe } from "fp-ts/function";
import {
  decodePayoutPaidEvent,
  decodePayoutReportRows,
  decodeReportRunEvent,
  parseCSVAndDecode,
} from "./webhook/webhook.decoders";
import { fold } from "fp-ts/TaskEither";
import { task, taskEither } from "fp-ts";
import { Stripe } from "stripe";
import { DynamoDB } from "aws-sdk";
import { getStripeApiKey } from "./utils/config";
import * as AWS from "aws-sdk";

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const runenv: any = process.env.RUNENV;
const transactionsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const committeeTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
let stripe: Stripe;
let stripeApiKey: string;
let dynamoDB: DynamoDB;

export default async (
  event: APIGatewayEvent
): Promise<APIGatewayProxyResult> => {
  if (!stripeApiKey || !stripe) {
    console.log("Setting up configuration");
    stripeApiKey = await getStripeApiKey(runenv);
    stripe = new Stripe(stripeApiKey, {
      apiVersion: "2020-08-27",
    });
    console.log("Configuration values have been set");
  }

  const payload = JSON.parse(event.body);

  switch (payload?.type) {
    case "reporting.report_run.succeeded":
      console.log("reporting.report_type.succeeded event happened");
      return await pipe(
        decodeReportRunEvent(payload),
        taskEither.map(reportEventToUrl),
        taskEither.chain(getReportAndDecode(stripeApiKey)),
        taskEither.chain(parseCSVAndDecode),
        taskEither.chain(decodePayoutReportRows),
        fold(
          (error) => task.of(error.toResponse()),
          () => task.of(successResponse)
        )
      )();
    case "payout.paid":
      return await pipe(
        decodePayoutPaidEvent(payload),
        fold(
          (error) => task.of(error.toResponse()),
          () => task.of(successResponse)
        )
      )();
    default:
      console.log(`Unhandled event type ${payload?.type}`);
  }
};
