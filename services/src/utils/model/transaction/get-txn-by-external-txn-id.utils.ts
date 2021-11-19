import { DynamoDB } from "aws-sdk";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../../application-error";
import { pipe } from "fp-ts/function";
import { StatusCodes } from "http-status-codes";
import { taskEither } from "fp-ts";
import { QueryInput } from "aws-sdk/clients/dynamodb";

const logPrefix = "Get Transaction by External Transaction Id";

export interface IGetTxnByExternalTxnIdArgs {
  committeeId: string;
  externalTransactionId: string;
}

export const externalTxnExistsUnsafe =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async ({
    committeeId,
    externalTransactionId,
  }: IGetTxnByExternalTxnIdArgs): Promise<boolean> => {
    const query: QueryInput = {
      TableName: txnsTableName,
      IndexName: "TransactionsByExternalTransactionId",
      KeyConditionExpression:
        "committeeId = :committeeId AND externalTransactionId = :externalTransactionId",
      ExpressionAttributeValues: {
        ":committeeId": { S: committeeId },
        ":externalTransactionId": { S: externalTransactionId },
      },
    };

    const res = await dynamoDB.query(query).promise();

    const marshalledRes = res.Items.map((item) => {
      return DynamoDB.Converter.unmarshall(item);
    });

    return marshalledRes.length > 0;
  };

export const externalTxnExists =
  (txnsTable: string) =>
  (dynamoDB: DynamoDB) =>
  (args: IGetTxnByExternalTxnIdArgs): TaskEither<ApplicationError, boolean> =>
    pipe(
      tryCatch<ApplicationError, boolean>(
        () => externalTxnExistsUnsafe(txnsTable)(dynamoDB)(args),
        (e) =>
          new ApplicationError(
            "Get transactions by ExternalTxn Transactions ID request failed",
            e,
            StatusCodes.INTERNAL_SERVER_ERROR
          )
      )
    );

export const isNewExternalTxn =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string) =>
  (externalTransactionId: string): TaskEither<ApplicationError, boolean> =>
    pipe(
      externalTxnExists(txnsTableName)(dynamoDB)({
        committeeId,
        externalTransactionId,
      }),
      taskEither.map((res) => !res)
    );
