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
import { PathReporter } from "io-ts/PathReporter";
import * as dotenv from "dotenv";
import { sendContribSuccessMsgs } from "./pipes/send-contrib-success-msgs.pipe";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const sqs = new AWS.SQS({ apiVersion: "2012-11-05" });
const dynamoDB = new DynamoDB();
const sqsUrl: any = process.env.SQSQUEUE;
const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;

export default async (event: DynamoDBStreamEvent): Promise<EffectMetadata> => {
  console.log(JSON.stringify(event));
  console.log("initiating ddb update loop");

  for (const stream of event.Records) {
    const record = stream.dynamodb;
    switch (stream.eventName) {
      case "INSERT":
        console.log("INSERT event emitted");
        return await handleInsert(sqsUrl)(committeesTableName)(dynamoDB)(
          parseStreamRecord(record)
        );
      case "MODIFY":
        const newImage = AWS.DynamoDB.Converter.unmarshall(record.NewImage);
        const oldImage = AWS.DynamoDB.Converter.unmarshall(record.NewImage);
        console.log("modify event data new image:", JSON.stringify(newImage));
        console.log("modify event data old image:", JSON.stringify(oldImage));
        console.log("MODIFY event emitted");
        return;
      default:
        return;
    }
  }
};

const parseStreamRecord = (record: any): ITransaction => {
  const unmarshalledTxn = AWS.DynamoDB.Converter.unmarshall(record.NewImage);
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
}

enum Status {
  SUCCESS = "success",
  FAILURE = "failure",
}

interface EffectMetadata {
  status: Status;
  ddbEventName: string;
  message: string;
  effect: Effect;
}

const successfulSend: EffectMetadata = {
  status: Status.SUCCESS,
  ddbEventName: "INSERT",
  message: "message send succeeded",
  effect: Effect.SQS_RECEIPT_MESSAGE_SENT,
};

const failedSend: EffectMetadata = {
  status: Status.FAILURE,
  ddbEventName: "INSERT",
  message: "message send failed",
  effect: Effect.SQS_RECEIPT_MESSAGE_SENT,
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
