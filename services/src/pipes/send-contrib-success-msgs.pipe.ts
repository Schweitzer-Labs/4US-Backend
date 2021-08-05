import { ITransaction } from "../queries/search-transactions.decoder";
import {
  getCommitteeById,
  ICommittee,
} from "../queries/get-committee-by-id.query";
import { SendMessageRequest } from "aws-sdk/clients/sqs";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { task, taskEither } from "fp-ts";
import { DynamoDB, SQS } from "aws-sdk";
import { pipe } from "fp-ts/function";

export const sendContribSuccessMsgs =
  (committeesTableName: string) =>
  (ddb: DynamoDB) =>
  (sqsUrl: string) =>
  (sqs: SQS) =>
  (txn: ITransaction) =>
    pipe(
      getCommitteeById(committeesTableName)(ddb)(txn.committeeId),
      taskEither.map(formatMessage(sqsUrl)(txn)),
      taskEither.chain(sendMessage(sqs))
    );

const formatMessage =
  (sqsUrl: string) =>
  (txn: ITransaction) =>
  (committee: ICommittee): SendMessageRequest => {
    console.log(
      "formatMessage called",
      sqsUrl,
      JSON.stringify(txn),
      JSON.stringify(committee)
    );
    const { tzDatabaseName, emailAddresses, committeeName } = committee;
    return {
      MessageAttributes: {
        committeeEmailAddress: {
          DataType: "String",
          StringValue: emailAddresses,
        },
        committeeTzDatabaseName: {
          DataType: "String",
          StringValue: tzDatabaseName,
        },
        committeeName: {
          DataType: "String",
          StringValue: committeeName,
        },
      },
      MessageBody: JSON.stringify(txn),
      MessageDeduplicationId: txn.id,
      MessageGroupId: txn.committeeId,
      QueueUrl: sqsUrl,
    };
  };
const sendMessage =
  (sqs: SQS) =>
  (message: SendMessageRequest): TaskEither<ApplicationError, any> => {
    console.log("message send attempted");
    return taskEither.tryCatch(
      () => sqs.sendMessage(message).promise(),
      (e) => new ApplicationError("SQS send failed", e)
    );
  };
