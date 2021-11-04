import { ITransaction } from "../queries/search-transactions.decoder";
import { getCommitteeById } from "../queries/get-committee-by-id.query";
import { SendMessageRequest } from "aws-sdk/clients/sqs";
import { taskEither } from "fp-ts";
import { DynamoDB, SQS } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { sendMessage } from "../utils/send-sqs.utils";
import { ICommittee } from "../types/committee.type";

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
