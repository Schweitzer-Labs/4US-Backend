import * as AWS from "aws-sdk";
import { DynamoDBStreamEvent } from "aws-lambda";
import { SendMessageRequest } from "aws-sdk/clients/sqs";
import {
  ITransaction,
  Transaction,
  Transactions,
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
import { taskEither } from "fp-ts";
import { PathReporter } from "io-ts/PathReporter";

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
AWS.config.update({ region: "us-east-1" });
const sqs = new AWS.SQS({ apiVersion: "2012-11-05" });
const dynamoDB = new DynamoDB();
const runenv: any = process.env.RUNENV;
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
        const res = await handleInsert(committeesTableName)(dynamoDB)(
          eitherTxn.right
        );
        console.log(res);
        console.log("transaction insert event emitted");
        break;
      // return await sendQueue(eitherTxn.right);
      case "MODIFY":
        console.log("transaction modify event emitted");
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
  action: Effect;
}

const handleInsert =
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (txn: ITransaction) => {
    if (txn.source === Source.DONATE_FORM) {
      const res = await pipe(
        getCommitteeById(committeeTableName)(dynamoDB)(txn.committeeId),
        taskEither.map(formatMessage(txn))
      )();
    }
  };

const formatMessage =
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
      QueueUrl: process.env.SQSQUEUE,
    };
  };

const sendMessage = async (message: SendMessageRequest) => {};
