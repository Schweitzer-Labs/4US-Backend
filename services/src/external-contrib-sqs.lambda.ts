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
import { ActBlueSQSMsgBody } from "./pipes/actblue-to-sqs.pipe";
import { actBlueCSVMetadataToTypedData } from "./clients/actblue/actblue.client";
import { syncActBlue } from "./external-data/act-blue.external-data";
import { strToJSON } from "./utils/str-to-json.util";
import { mLog } from "./utils/m-log.utils";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const dynamoDB = new DynamoDB();

const billableEventsTable: any = process.env.BILLABLE_EVENTS_DDB_TABLE_NAME;
const transactionsTable: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const committeesTable: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const donorsTable: any = process.env.DONORS_DDB_TABLE_NAME;
const rulesTable: any = process.env.RULES_DDB_TABLE_NAME;
const runenv: any = process.env.RUNENV;

let stripeApiKey: string;
let stripe: Stripe;
let lnUsername: string;
let lnPassword: string;

const ps = new AWS.SSM();

export default async (event: SQSEvent): Promise<any> => {
  if (!stripeApiKey || !stripe || !lnUsername || !lnPassword) {
    console.log("Setting up configuration");
    stripeApiKey = await getStripeApiKey(ps)(runenv);
    lnUsername = await getLNUsername(ps)(runenv);
    lnPassword = await getLNPassword(ps)(runenv);
    stripe = new Stripe(stripeApiKey, {
      apiVersion: "2020-08-27",
    });
    console.log("Configuration values have been set.");
  }
  const lexisNexisConfig: ILexisNexisConfig = {
    username: lnUsername,
    password: lnPassword,
  };

  for (const record of event.Records) {
    console.log("record loop begins", record);
    const res = await pipe(
      strToJSON(record.body),
      taskEither.chain(mLog("body decodes")),
      taskEither.chain(decodeRawData("Committee Id")(ActBlueSQSMsgBody)),
      taskEither.chain((msg) =>
        pipe(
          getCommitteeById(committeesTable)(dynamoDB)(msg.committeeId),
          taskEither.map((com) => com.actBlueAPICredentials),
          taskEither.chain(actBlueCSVMetadataToTypedData(msg.csvId)),
          taskEither.chain(
            syncActBlue({
              committeesTable,
              billableEventsTable,
              donorsTable,
              transactionsTable,
              rulesTable,
              dynamoDB,
              lexisNexisConfig,
              stripe,
            })(msg.committeeId)
          )
        )
      )
    )();

    if (isLeft(res)) throw res.left;
    console.log("External Contribs synched", JSON.stringify(res.right));
  }

  return "External Contribs SQS invocation complete";
};
