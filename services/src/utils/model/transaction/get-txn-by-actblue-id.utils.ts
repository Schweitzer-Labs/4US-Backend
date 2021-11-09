import { DynamoDB } from "aws-sdk";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../../application-error";
import { pipe } from "fp-ts/function";
import { StatusCodes } from "http-status-codes";
import { taskEither } from "fp-ts";
import * as t from "io-ts";

import { validateDDBResponse } from "../../ddb.utils";
import { QueryInput } from "aws-sdk/clients/dynamodb";

const logPrefix = "Get Transaction by ActBlue Transaction Id";

interface GetTxnByActBlueIdArgs {
  committeeId: string;
  actBlueTransactionId: string;
}

export const actBlueTxnExistsUnsafe =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async ({
    committeeId,
    actBlueTransactionId,
  }: GetTxnByActBlueIdArgs): Promise<boolean> => {
    const query: QueryInput = {
      TableName: txnsTableName,
      IndexName: "TransactionsByActBlueTransactionId",
      KeyConditionExpression:
        "committeeId = :committeeId AND actBlueTransactionId = :actBlueTransactionId",
      ExpressionAttributeValues: {
        ":committeeId": { S: committeeId },
        ":actBlueTransactionId": { S: actBlueTransactionId },
      },
    };

    const res = await dynamoDB.query(query).promise();

    const marshalledRes = res.Items.map((item) => {
      return DynamoDB.Converter.unmarshall(item);
    });

    return marshalledRes.length > 0;
  };

export const actBlueTxnExists =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (args: GetTxnByActBlueIdArgs): TaskEither<ApplicationError, boolean> =>
    pipe(
      tryCatch<ApplicationError, boolean>(
        () => actBlueTxnExistsUnsafe(txnsTableName)(dynamoDB)(args),
        (e) =>
          new ApplicationError(
            "Get transactions by ActBlue Transactions ID request failed",
            e,
            StatusCodes.INTERNAL_SERVER_ERROR
          )
      )
    );

export const isNewActBlueTxn =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (args: GetTxnByActBlueIdArgs): TaskEither<ApplicationError, boolean> =>
    pipe(
      actBlueTxnExists(txnsTableName)(dynamoDB)(args),
      taskEither.map((res) => !res)
    );
