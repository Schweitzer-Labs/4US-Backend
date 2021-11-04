import { SQSEvent } from "aws-lambda";
import { verifiedTxnToStrato } from "./pipes/verified-txn-to-strato.pipe";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import {
  getStratoENodeUrl,
  getStratoNodeUrl,
  getStratoOAuthClientId,
  getStratoOauthClientSecret,
  getStratoOAuthOpenIdDiscoveryUrl,
} from "./utils/config";
import { initStratoConfig } from "./clients/dapp/dapp.decoders";
import { pipe } from "fp-ts/function";
import { decodeRawData } from "./utils/decode-raw-data.util";
import { Transaction } from "./types/transaction.type";
import { taskEither } from "fp-ts";
import { isLeft } from "fp-ts/Either";

dotenv.config();

const logPrefix = "Strato SQS Transaction";

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const ps = new AWS.SSM();

const ddb = new DynamoDB();
const comTable: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const txnTable: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const runEnv: any = process.env.RUNENV;

let nodeUrl: string;
let eNodeUrl: string;
let oauthClientId: string;
let oauthClientSecret: string;
let oauthOpenIdDiscoveryUrl: string;

export default async (event: SQSEvent, context): Promise<any> => {
  console.log("Strato SQS Payload", JSON.stringify(event));
  if (
    !nodeUrl ||
    !eNodeUrl ||
    !oauthClientId ||
    !oauthClientSecret ||
    !oauthOpenIdDiscoveryUrl
  ) {
    nodeUrl = await getStratoNodeUrl(ps)(runEnv);
    eNodeUrl = await getStratoENodeUrl(ps)(runEnv);
    oauthClientId = await getStratoOAuthClientId(ps)(runEnv);
    oauthClientSecret = await getStratoOauthClientSecret(ps)(runEnv);
    oauthOpenIdDiscoveryUrl = await getStratoOAuthOpenIdDiscoveryUrl(ps)(
      runEnv
    );
  }

  const stratoConf = initStratoConfig({
    nodeUrl,
    eNodeUrl,
    oauthClientId,
    oauthClientSecret,
    oauthOpenIdDiscoveryUrl,
  });

  for (const record of event.Records) {
    const rawTxn = JSON.parse(record.body);
    const res = await pipe(
      decodeRawData(logPrefix)(Transaction)(rawTxn),
      taskEither.chain(verifiedTxnToStrato(stratoConf)(txnTable)(comTable)(ddb))
    )();

    if (isLeft(res)) {
      throw res.left;
    } else {
      console.log(
        "Txn successfully committed to strato",
        JSON.stringify(res.right)
      );
    }
  }

  return true;
};
