import { SQSEvent } from "aws-lambda";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import {
  getFinicityAppKey,
  getFinicityPartnerId,
  getFinicityPartnerSecret,
  getLNPassword,
  getLNUsername,
  getStripeApiKey,
} from "./utils/config";
import { pipe } from "fp-ts/function";
import { decodeRawData } from "./utils/decode-raw-data.util";
import { taskEither } from "fp-ts";
import { isLeft } from "fp-ts/Either";
import { FinicityConfig } from "./clients/finicity/finicity.decoders";
import { getCommitteeById } from "./utils/model/committee/get-committee-by-id.query";
import { syncCommittee } from "./pipes/finicity-bank-sync.pipe";
import * as t from "io-ts";
import { runReconcileOnCommittee } from "./demo/utils/run-rec.util";
import { Stripe } from "stripe";
import { ILexisNexisConfig } from "./clients/lexis-nexis/lexis-nexis.client";

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

let stripeApiKey: string;
let stripe: Stripe;
let lnUsername: string;
let lnPassword: string;

const ps = new AWS.SSM();

export default async (event: SQSEvent): Promise<any> => {
  // if (!stripeApiKey || !stripe || !lnUsername || !lnPassword) {
  //   console.log("Setting up configuration");
  //   stripeApiKey = await getStripeApiKey(ps)(runenv);
  //   lnUsername = await getLNUsername(ps)(runenv);
  //   lnPassword = await getLNPassword(ps)(runenv);
  //   stripe = new Stripe(stripeApiKey, {
  //     apiVersion: "2020-08-27",
  //   });
  //   console.log("Configuration values have been set.");
  // }
  // const instantIdConfig: ILexisNexisConfig = {
  //   username: lnUsername,
  //   password: lnPassword,
  // };
  //
  // for (const record of event.Records) {
  //   const res = await pipe(
  //     decodeRawData("Committee Id")(t.string)(record.body),
  //     taskEither.chain(getCommitteeById(comsTable)(ddb)),
  //     taskEither.chain(syncCommittee(finicityConfig)(txnTable)(ddb))
  //   )();
  //
  //   if (isLeft(recRes)) throw recRes.left;
  //   console.log("txns reconciled", JSON.stringify(recRes.right));
  // }

  return "ActBlue SQS invocation complete";
};
