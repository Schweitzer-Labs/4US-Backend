import "reflect-metadata";
import { getLNPassword, getLNUsername, getStripeApiKey } from "./utils/config";
import { Stripe } from "stripe";
import * as dotenv from "dotenv";
import { platformContribute } from "./pipes/platform-contribute.pipe";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";
import { successResponse } from "./utils/success-response";
import { errorResponse } from "./utils/error-response.utils";
import { StatusCodes } from "http-status-codes";
import { ILexisNexisConfig } from "./clients/lexis-nexis/lexis-nexis.client";
import { headers } from "./utils/headers";

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
const corsOrigin = process.env.CORS_ORIGIN;

const ps = new AWS.SSM();

export default async (event: any) => {
    console.log('bluelink webhook called')
    console.log(JSON.stringify(event))
    return {
        ...successResponse,
        headers: headers(corsOrigin),
    }
};
