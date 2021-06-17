import "reflect-metadata";
import { getLNPassword, getLNUsername, getStripeApiKey } from "./utils/config";
import { Stripe } from "stripe";
import { IInstantIdConfig } from "./clients/lexis-nexis/lexis-nexis.client";
import * as dotenv from "dotenv";
import { platformContribute } from "./pipes/platform-contribute.pipe";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";
import { successResponse } from "./utils/success-response";
import { errorResponse } from "./utils/error-response.utils";
import { StatusCodes } from "http-status-codes";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const dynamoDB = new DynamoDB();

const billableEventsTableName: any = process.env.BILLABLE_EVENTS_DDB_TABLE_NAME;
const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const donorsTableName: any = process.env.DONORS_DDB_TABLE_NAME;
const rulesTableName: any = process.env.RULES_DDB_TABLE_NAME;
const runenv: any = process.env.RUNENV;

let stripeApiKey: string;
let stripe: Stripe;
let lnUsername: string;
let lnPassword: string;

export default async (event: any) => {
  if (!stripeApiKey || !stripe || !lnUsername || !lnPassword) {
    console.log("Setting up configuration");
    stripeApiKey = await getStripeApiKey(runenv);
    lnUsername = await getLNUsername(runenv);
    lnPassword = await getLNPassword(runenv);
    stripe = new Stripe(stripeApiKey, {
      apiVersion: "2020-08-27",
    });
    console.log("Configuration values have been set");
  }
  const instantIdConfig: IInstantIdConfig = {
    username: lnUsername,
    password: lnPassword,
  };
  console.log("Initiating pipe");
  return await pipe(
    platformContribute(billableEventsTableName)(donorsTableName)(
      committeesTableName
    )(txnsTableName)(rulesTableName)(dynamoDB)(stripe)(instantIdConfig)(event),
    taskEither.fold(
      (error) => {
        const message = error.message;
        const remaining = error?.data?.remaining
          ? { remaining: error.data.remaining }
          : {};

        const res = {
          statusCode: error.statusCode || StatusCodes.UNAUTHORIZED,
          body: {
            message,
            ...remaining,
          },
        };

        return task.of(errorResponse(res));
      },
      (result) => task.of(successResponse)
    )
  )();
};
