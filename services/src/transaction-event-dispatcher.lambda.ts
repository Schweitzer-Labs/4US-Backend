import * as AWS from "aws-sdk";
import { DynamoDBStreamEvent } from "aws-lambda";

import {
  ITransaction,
  Transaction,
} from "./queries/search-transactions.decoder";
import { isLeft } from "fp-ts/Either";
import { ApplicationError } from "./utils/application-error";
import { Source } from "./utils/enums/source.enum";
import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";
import * as dotenv from "dotenv";
import { sendContribSuccessMsgs } from "./pipes/send-contrib-success-msgs.pipe";
import { validateDDBResponse } from "./repositories/ddb.utils";
import { auditTxnStream, DdbEventName } from "./pipes/audit-txn-stream.pipe";

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
        const insertedTxn = await streamRecordToTxn(record.NewImage);
        return await handleInsert(sqsUrl)(committeesTableName)(dynamoDB)(
          insertedTxn
        );
      case DdbEventName.MODIFY:
        return await pipe(
          auditTxnStream(auditLogsTableName)(dynamoDB)(DdbEventName.MODIFY)(
            record.OldImage
          )(record.NewImage),
          taskEither.fold(
            (err) => task.of(failedAuditPut(DdbEventName.MODIFY)(err)),
            (log) => task.of(successfulAuditPut(DdbEventName.MODIFY)(log))
          )
        )();
      default:
        return;
    }
  }
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

      return pipe(
        sendContribSuccessMsgs(committeesTableName)(dynamoDB)(sqsUrl)(sqs)(txn),
        taskEither.fold(
          () => task.of(failedSend),
          () => task.of(successfulSend)
        )
      )();
    }
  };
