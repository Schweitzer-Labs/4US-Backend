import * as AWS from "aws-sdk";
import { DynamoDBStreamEvent } from "aws-lambda";

import {
  currentVersion,
  ITransaction,
  Transaction,
} from "./queries/search-transactions.decoder";
import { isLeft } from "fp-ts/Either";
import { ApplicationError } from "./utils/application-error";
import { Source } from "./utils/enums/source.enum";
import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";
import { PathReporter } from "io-ts/PathReporter";
import * as dotenv from "dotenv";
import { sendContribSuccessMsgs } from "./pipes/send-contrib-success-msgs.pipe";
import { now } from "./utils/time.utils";
import { ITxnAuditLog } from "./queries/get-audit-logs.decoders";
import { putTxnAuditLogAndDecode } from "./utils/model/put-txn-audit-log.utils";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const sqs = new AWS.SQS({ apiVersion: "2012-11-05" });
const dynamoDB = new DynamoDB();
const sqsUrl: any = process.env.SQSQUEUE;
const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const auditLogsTableName: any = process.env.AUDIT_LOGS_DDB_TABLE_NAME;

export default async (event: DynamoDBStreamEvent): Promise<EffectMetadata> => {
  console.log(JSON.stringify(event));
  console.log("initiating ddb update loop");

  for (const stream of event.Records) {
    const record = stream.dynamodb;
    switch (stream.eventName) {
      case DdbEventName.INSERT:
        console.log("INSERT event emitted");
        const insertRes = await handleInsert(sqsUrl)(committeesTableName)(
          dynamoDB
        )(streamRecordToTxn(record.NewImage));
        console.log(insertRes);
        return insertRes;
      case DdbEventName.MODIFY:
        const newImageTxn = AWS.DynamoDB.Converter.unmarshall(record.NewImage);
        const oldImageTxn = AWS.DynamoDB.Converter.unmarshall(record.NewImage);
        const newTxn = streamRecordToTxn(newImageTxn);
        const oldTxn = streamRecordToTxn(oldImageTxn);

        console.log("modified event called");
        console.log("new txn", JSON.stringify(newTxn));
        console.log("modified event called", JSON.stringify(oldTxn));
        const modifyRes = await handleModify(auditLogsTableName)(dynamoDB)(
          oldTxn
        )(newTxn);
        console.log(modifyRes);
        return modifyRes;
      default:
        return;
    }
  }
};

const streamRecordToTxn = (record: any): ITransaction => {
  const unmarshalledTxn = AWS.DynamoDB.Converter.unmarshall(record);
  console.log(
    "Transaction record unmarshalled",
    JSON.stringify(unmarshalledTxn)
  );
  const eitherTxn = Transaction.decode(unmarshalledTxn);

  if (isLeft(eitherTxn)) {
    throw new ApplicationError(
      "Invalid transaction data",
      PathReporter.report(eitherTxn)
    );
  }
  return eitherTxn.right;
};

enum Effect {
  SQS_RECEIPT_MESSAGE_SENT = "sqs_receipt_message_sent",
  DDB_AUDIT_LOG_RECORDED = "ddb_audit_log_recorded",
}

enum DdbEventName {
  MODIFY = "MODIFY",
  INSERT = "INSERT",
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
  (auditLog: ITxnAuditLog): EffectMetadata => ({
    status: Status.SUCCESS,
    ddbEventName: ddbEventName,
    message: "ddb audit log recorded succeeded",
    effect: Effect.DDB_AUDIT_LOG_RECORDED,
    metadata: JSON.stringify(auditLog),
  });

const failedAuditPut =
  (ddbEventName: DdbEventName) =>
  (auditLog: ITxnAuditLog): EffectMetadata => ({
    status: Status.FAILURE,
    ddbEventName: ddbEventName,
    message: "ddb audit log recorded failed",
    effect: Effect.DDB_AUDIT_LOG_RECORDED,
    metadata: JSON.stringify(auditLog),
  });

const handleModify =
  (txnAuditLogs: string) =>
  (ddb: DynamoDB) =>
  (oldTransaction: ITransaction) =>
  (newTransaction: ITransaction): Promise<EffectMetadata> => {
    const ddbEvent = DdbEventName.MODIFY;

    const auditLog: ITxnAuditLog = {
      committeeId: newTransaction.committeeId,
      id: newTransaction.id,
      newTransaction,
      oldTransaction,
      version: currentVersion,
      type: "Transaction",
      ddbEvent: ddbEvent,
      timestamp: now(),
    };

    return pipe(
      putTxnAuditLogAndDecode(txnAuditLogs)(ddb)(auditLog),
      taskEither.fold(
        () => task.of(successfulAuditPut(ddbEvent)(auditLog)),
        () => task.of(failedAuditPut(ddbEvent)(auditLog))
      )
    )();
  };

const handleInsert =
  (sqsUrl: string) =>
  (committeesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (txn: ITransaction): Promise<EffectMetadata> => {
    console.log("handleInsert called with transactions:", JSON.stringify(txn));
    if (txn.source === Source.DONATE_FORM) {
      console.log("donate form transaction recognized");
      console.log("initiating pipe");

      return pipe(
        sendContribSuccessMsgs(committeesTableName)(dynamoDB)(sqsUrl)(sqs)(txn),
        taskEither.fold(
          () => task.of(failedSend),
          () => task.of(successfulSend)
        )
      )();
    }
  };
