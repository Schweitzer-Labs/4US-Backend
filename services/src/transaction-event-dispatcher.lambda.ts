import * as AWS from "aws-sdk";
import { DynamoDBStreamEvent } from "aws-lambda";

import {
  ITransaction,
  Transaction,
} from "./queries/search-transactions.decoder";
import { isLeft } from "fp-ts/Either";
import { ApplicationError } from "./utils/application-error";
import { Source } from "./utils/enums/source.enum";
import { DynamoDB, DynamoDBStreams } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";
import * as dotenv from "dotenv";
import { sendContribSuccessMsgs } from "./pipes/send-contrib-success-msgs.pipe";
import { validateDDBResponse } from "./repositories/ddb.utils";
import { auditTxnStream, DdbEventName } from "./pipes/audit-txn-stream.pipe";
import {
  initStratoConfig,
  IStratoSDKConfig,
} from "./clients/dapp/dapp.decoders";
import {
  getStratoENodeUrl,
  getStratoNodeUrl,
  getStratoOAuthClientId,
  getStratoOauthClientSecret,
  getStratoOAuthOpenIdDiscoveryUrl,
} from "./utils/config";
import { refreshAggs } from "./pipes/refresh-aggs.pipe";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const ps = new AWS.SSM();

const sqs = new AWS.SQS({ apiVersion: "2012-11-05" });
const dynamoDB = new DynamoDB();
const sqsUrl: any = process.env.SQSQUEUE;
const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const auditLogsTableName: any = process.env.AUDIT_LOGS_DDB_TABLE_NAME;
const txnTable: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const aggsTable: any = process.env.AGGREGATES_DDB_TABLE_NAME;
const runEnv: any = process.env.RUNENV;

let nodeUrl: string;
let eNodeUrl: string;
let oauthClientId: string;
let oauthClientSecret: string;
let oauthOpenIdDiscoveryUrl: string;
let res: any;

export default async (event: DynamoDBStreamEvent): Promise<EffectMetadata> => {
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

  console.log(JSON.stringify(event));
  console.log("initiating ddb update loop");

  const handlerWithConf =
    handleStream(stratoConf)(auditLogsTableName)(committeesTableName)(txnTable)(
      dynamoDB
    );

  const resCol: EffectMetadata[] = [];
  for (const stream of event.Records) {
    resCol.push(await handlerWithConf(stream));
  }

  const exampleTxn: any = event.Records[0].dynamodb.NewImage;

  await pipe(
    taskEither.of(AWS.DynamoDB.Converter.unmarshall(exampleTxn)),
    taskEither.chain(validateDDBResponse("Txn")(Transaction)),
    taskEither.map((txn) => txn.committeeId),
    taskEither.chain(refreshAggs(aggsTable)(txnTable)(dynamoDB))
  )();

  return resCol[0];
};

const streamRecordToTxn = async (txnRecord: any): Promise<ITransaction> => {
  const unmarshalledTxn = AWS.DynamoDB.Converter.unmarshall(txnRecord);
  console.log(
    "Transaction record unmarshalled",
    JSON.stringify(unmarshalledTxn)
  );

  const eitherTxn = await validateDDBResponse("txn inserted")(Transaction)(
    unmarshalledTxn
  )();

  if (isLeft(eitherTxn)) {
    throw new ApplicationError("Invalid transaction data", {});
  }
  return eitherTxn.right;
};

enum Effect {
  SQS_RECEIPT_MESSAGE_SENT = "sqs_receipt_message_sent",
  DDB_AUDIT_LOG_RECORDED = "ddb_audit_log_recorded",
}

enum Status {
  SUCCESS = "success",
  FAILURE = "failure",
}

interface EffectMetadata {
  status: Status;
  ddbEventName: DdbEventName;
  message: string;
  effect: Effect;
  metadata?: any;
}

const successfulSend: EffectMetadata = {
  status: Status.SUCCESS,
  ddbEventName: DdbEventName.INSERT,
  message: "message send succeeded",
  effect: Effect.SQS_RECEIPT_MESSAGE_SENT,
};

const failedSend: EffectMetadata = {
  status: Status.FAILURE,
  ddbEventName: DdbEventName.INSERT,
  message: "message send failed",
  effect: Effect.SQS_RECEIPT_MESSAGE_SENT,
};

const successfulAuditPut =
  (ddbEventName: DdbEventName) =>
  (res: any): EffectMetadata => ({
    status: Status.SUCCESS,
    ddbEventName: ddbEventName,
    message: "ddb audit log recorded succeeded",
    effect: Effect.DDB_AUDIT_LOG_RECORDED,
    metadata: JSON.stringify(res),
  });

const failedAuditPut =
  (ddbEventName: DdbEventName) =>
  (res: any): EffectMetadata => ({
    status: Status.FAILURE,
    ddbEventName: ddbEventName,
    message: "ddb audit log recorded failed",
    effect: Effect.DDB_AUDIT_LOG_RECORDED,
    metadata: JSON.stringify(res),
  });

const handleInsert =
  (sqsUrl: string) =>
  (committeesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (txn: ITransaction): Promise<EffectMetadata> => {
    console.log("handleInsert called with transactions:", JSON.stringify(txn));
    if (txn.source === Source.DONATE_FORM) {
      console.log("donate form transaction recognized");
      console.log("initiating pipe");

      const res = await pipe(
        sendContribSuccessMsgs(committeesTableName)(dynamoDB)(sqsUrl)(sqs)(txn),
        taskEither.fold(
          () => task.of(failedSend),
          () => task.of(successfulSend)
        )
      )();

      return res;
    }
  };

const handleStream =
  (stratoConf: IStratoSDKConfig) =>
  (aLogsTableName: string) =>
  (comTable: string) =>
  (txnsTable: string) =>
  (ddb: DynamoDB) =>
  async (stream: any): Promise<EffectMetadata> => {
    const record = stream.dynamodb;
    let res: any;
    switch (stream.eventName) {
      case DdbEventName.INSERT:
        console.log("INSERT event emitted");
        const insertedTxn = await streamRecordToTxn(record.NewImage);
        res = await handleInsert(sqsUrl)(committeesTableName)(dynamoDB)(
          insertedTxn
        );
        break;
      case DdbEventName.MODIFY:
        console.log("modify call");
        console.log("record: ", JSON.stringify(record));
        res = await pipe(
          auditTxnStream(stratoConf)(comTable)(txnsTable)(aLogsTableName)(ddb)(
            DdbEventName.MODIFY
          )(record.OldImage)(record.NewImage),
          taskEither.fold(
            (err) => task.of(failedAuditPut(DdbEventName.MODIFY)(err)),
            (log) => task.of(successfulAuditPut(DdbEventName.MODIFY)(log))
          )
        )();
    }

    return res;
  };
