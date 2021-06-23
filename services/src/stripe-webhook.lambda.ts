import { APIGatewayEvent, APIGatewayProxyResult } from "aws-lambda";
import { successResponse } from "./utils/success-response";
import {
  getReportAndDecode,
  runReportAndDecode,
  syncPayout,
} from "./request-report";
import { pipe } from "fp-ts/function";
import {
  decodePayoutPaidEvent,
  decodePayoutReportRows,
  decodeReportRunEvent,
  parseCSVAndDecode,
  reportEventToUrl,
  reportEventToStripeAccount,
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
const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
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
      await handleReportRunSucceeded(payload);
      return;
    case "payout.paid":
      return await handlePayoutPaid(payload);
    default:
      console.log(`Unhandled event type ${payload?.type}`);
  }
};

export const handleReportRunSucceeded = async (payload: unknown) =>
  await pipe(
    decodeReportRunEvent(payload),
    taskEither.chain((reportEvent) =>
      pipe(
        taskEither.of(reportEventToUrl(reportEvent)),
        taskEither.chain(getReportAndDecode(stripeApiKey)),
        taskEither.chain(parseCSVAndDecode),
        taskEither.chain(decodePayoutReportRows),
        taskEither.chain(
          syncPayout(txnsTableName)(committeeTableName)(dynamoDB)(
            reportEventToStripeAccount(reportEvent)
          )
        )
      )
    ),
    fold(
      (error) => task.of(error.toResponse()),
      () => task.of(successResponse)
    )
  )();

export const handlePayoutPaid = async (payload: unknown) =>
  await pipe(
    decodePayoutPaidEvent(payload),
    taskEither.map((res) => res.account),
    taskEither.chain(runReportAndDecode(stripe)),
    fold(
      (error) => task.of(error.toResponse()),
      () => task.of(successResponse)
    )
  )();
