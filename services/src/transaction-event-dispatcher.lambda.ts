import * as AWS from "aws-sdk";
import { DynamoDBStreamEvent } from "aws-lambda";

import {
  ITransaction,
  Transaction,
} from "./queries/search-transactions.decoder";
import { isLeft } from "fp-ts/Either";
import { ApplicationError } from "./utils/application-error";
import { Source } from "./utils/enums/source.enum";
import { DynamoDB, SQS } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";
import * as dotenv from "dotenv";
import { sendContribSuccessMsgs } from "./pipes/send-contrib-success-msgs.pipe";
import { validateDDBResponse } from "./repositories/ddb.utils";
import { auditTxnStream, DdbEventName } from "./pipes/audit-txn-stream.pipe";
import { refreshAggs } from "./pipes/refresh-aggs.pipe";
import { TaskEither } from "fp-ts/TaskEither";
import { IAggs } from "./queries/get-aggs.decoders";
import { DynamoDBRecord } from "aws-lambda/trigger/dynamodb-stream";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const sqs = new AWS.SQS({ apiVersion: "2012-11-05" });
const dynamoDB = new DynamoDB();
// @ToDo rename SQSQUEUE to EMAILER_QUEUE
const successMsgSQSUrl: any = process.env.SQSQUEUE;
const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const auditLogsTableName: any = process.env.AUDIT_LOGS_DDB_TABLE_NAME;
const txnTable: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const aggsTable: any = process.env.AGGREGATES_DDB_TABLE_NAME;
const stratoSQSUrl: any = process.env.STRATO_SQS_URL;

export default async (event: DynamoDBStreamEvent): Promise<boolean> => {
  console.log(JSON.stringify(event));
  console.log("initiating ddb update loop");

  const handlerWithConf =
    handleStream(successMsgSQSUrl)(stratoSQSUrl)(sqs)(auditLogsTableName)(
      dynamoDB
    );

  const resCol: EffectMetadata[] = [];
  for (const stream of event.Records) {
    resCol.push(await handlerWithConf(stream));
  }

  const newTxn: any = event.Records[0].dynamodb.NewImage;
  const oldTxn: any = event.Records[0].dynamodb.OldImage;
  const txn = newTxn || oldTxn;

  console.log(JSON.stringify(resCol));

  await txnToAggsUpdate(aggsTable)(txnTable)(dynamoDB)(txn)();

  return true;
};

const handleInsert =
  (successMsgSQSUrl: string) =>
  (sqs: SQS) =>
  (committeesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (txn: ITransaction): Promise<EffectMetadata> => {
    console.log("handleInsert called with transactions:", JSON.stringify(txn));
    if (txn.source === Source.DONATE_FORM) {
      console.log("donate form transaction recognized");
      console.log("initiating pipe");

      const res = await pipe(
        sendContribSuccessMsgs(committeesTableName)(dynamoDB)(successMsgSQSUrl)(
          sqs
        )(txn),
        taskEither.fold(
          () => task.of(failedSend),
          () => task.of(successfulSend)
        )
      )();

      return res;
    }
  };

const handleStream =
  (successMsgSQSUrl: string) =>
  (stratoSQSUrl: string) =>
  (sqs: SQS) =>
  (aLogsTableName: string) =>
  (ddb: DynamoDB) =>
  async (ddbRecord: DynamoDBRecord): Promise<EffectMetadata> => {
    const record = ddbRecord.dynamodb;
    let res: any;
    switch (ddbRecord.eventName) {
      case DdbEventName.INSERT:
        console.log("INSERT event emitted");
        const insertedTxn = await streamRecordToTxn(record.NewImage);
        res = await handleInsert(successMsgSQSUrl)(sqs)(committeesTableName)(
          ddb
        )(insertedTxn);
        break;
      case DdbEventName.MODIFY:
        console.log("MODIFY event emitted");
        console.log("record: ", JSON.stringify(record));
        res = await pipe(
          auditTxnStream(stratoSQSUrl)(sqs)(aLogsTableName)(ddb)(
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

const txnToAggsUpdate =
  (aggsTable: string) =>
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (txn: ITransaction): TaskEither<ApplicationError, IAggs> =>
    pipe(
      taskEither.of(txn.committeeId),
      taskEither.chain(refreshAggs(aggsTable)(txnTable)(ddb))
    );

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
