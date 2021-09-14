import { SQSEvent } from "aws-lambda";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import {
  getFinicityAppKey,
  getFinicityPartnerId,
  getFinicityPartnerSecret,
} from "./utils/config";
import { pipe } from "fp-ts/function";
import { decodeRawData } from "./utils/decode-raw-data.util";
import { taskEither } from "fp-ts";
import { isLeft } from "fp-ts/Either";
import { FinicityConfig } from "./clients/finicity/finicity.decoders";
import { getCommitteeById } from "./queries/get-committee-by-id.query";
import { syncCommittee } from "./pipes/finicity-bank-sync.pipe";
import * as t from "io-ts";
import { ApplicationError } from "./utils/application-error";
import { runReconcileOnCommittee } from "./demo/utils/run-rec.util";
dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const ddb = new DynamoDB();
const parameterStore = new AWS.SSM();
const txnTable: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const comsTable: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const runenv: any = process.env.RUNENV;

let partnerId: string;
let partnerSecret: string;
let appKey: string;
let finicityConfig: FinicityConfig;

export default async (event: SQSEvent): Promise<any> => {
  if (!partnerId || !partnerSecret || !appKey) {
    partnerId = await getFinicityPartnerId(parameterStore)(runenv);
    partnerSecret = await getFinicityPartnerSecret(parameterStore)(runenv);
    appKey = await getFinicityAppKey(parameterStore)(runenv);

    finicityConfig = {
      partnerId,
      partnerSecret,
      appKey,
    };
  }
  for (const record of event.Records) {
    const res = await pipe(
      decodeRawData("Committee Id")(t.string)(record.body),
      taskEither.chain(getCommitteeById(comsTable)(ddb)),
      taskEither.chain(syncCommittee(finicityConfig)(txnTable)(ddb))
    )();

    if (isLeft(res)) throw res.left;

    const res2 = res.right.map((fn) => fn());

    const res3 = await Promise.all(res2);
    res3.map((val) => {
      if (isLeft(val)) {
        throw new ApplicationError("Bank Sync Failed", val);
      } else {
        console.log("txn synced: ", JSON.stringify(val.right));
      }
    });

    const recRes = await pipe(
      decodeRawData("Committee Id")(t.string)(record.body),
      taskEither.chain(runReconcileOnCommittee(txnTable)(ddb))
    )();

    if (isLeft(recRes)) throw recRes.left;
    console.log("txns reconciled", JSON.stringify(recRes.right));
  }

  return true;
};
