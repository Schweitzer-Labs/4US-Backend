import * as AWS from "aws-sdk";
import { DynamoDBStreamEvent } from "aws-lambda";
import { SendMessageRequest } from "aws-sdk/clients/sqs";
import {
  ITransaction,
  Transaction,
} from "./queries/search-transactions.decoder";
import { isLeft } from "fp-ts/Either";
import { ApplicationError } from "./utils/application-error";
import { Source } from "./utils/enums/source.enum";
import { DynamoDB } from "aws-sdk";
import {
  getCommitteeById,
  ICommittee,
} from "./queries/get-committee-by-id.query";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";
import { PathReporter } from "io-ts/PathReporter";
import { TaskEither } from "fp-ts/TaskEither";

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
AWS.config.update({ region: "us-east-1" });
const sqs = new AWS.SQS({ apiVersion: "2012-11-05" });
const dynamoDB = new DynamoDB();
const runenv: any = process.env.RUNENV;
const sqsUrl: any = process.env.SQSQUEUE;
const committeesTableName: any = `committees-${runenv}`;

export default async (event: DynamoDBStreamEvent, context): Promise<any> => {
  for (const stream of event.Records) {
    const record = stream.dynamodb;
    const unmarshalledTxn = AWS.DynamoDB.Converter.unmarshall(record.NewImage);
    const eitherTxn = Transaction.decode(unmarshalledTxn);
    PathReporter.report;
    if (isLeft(eitherTxn)) {
      return new ApplicationError(
        "Invalid transaction data",
        PathReporter.report(eitherTxn)
      );
    }

    switch (stream.eventName) {
      case "INSERT":
        return await handleInsert(sqsUrl)(committeesTableName)(dynamoDB)(
          eitherTxn.right
        );
      case "MODIFY":
        return;
      default:
        return;
    }
  }
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
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (txn: ITransaction): Promise<EffectMetadata> => {
    if (txn.source === Source.DONATE_FORM) {
      return await pipe(
        getCommitteeById(committeeTableName)(dynamoDB)(txn.committeeId),
        taskEither.map(formatMessage(sqsUrl)(txn)),
        taskEither.chain(sendMessage),
        taskEither.fold(
          () => task.of(failedSend),
          () => task.of(successfulSend)
        )
      )();
    }
  };

const formatMessage =
  (sqsUrl: string) =>
  (txn: ITransaction) =>
  (committee: ICommittee): SendMessageRequest => {
    const { tzDatabaseName, emailAddresses } = committee;
    return {
      MessageAttributes: {
        committeeEmailAddress: {
          DataType: "String",
          StringValue: emailAddresses,
        },
        tzcommittee: {
          DataType: "String",
          StringValue: tzDatabaseName,
        },
      },
      MessageBody: JSON.stringify(txn),
      MessageDeduplicationId: txn.id,
      MessageGroupId: txn.committeeId,
      QueueUrl: sqsUrl,
    };
  };
const sendMessage = (
  message: SendMessageRequest
): TaskEither<ApplicationError, any> => {
  return taskEither.tryCatch(
    () => sqs.sendMessage(message).promise(),
    (e) => new ApplicationError("SQS send failed", e)
  );
};
