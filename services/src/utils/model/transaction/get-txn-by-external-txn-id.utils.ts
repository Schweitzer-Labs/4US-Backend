import { DynamoDB } from "aws-sdk";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../../application-error";
import { pipe } from "fp-ts/function";
import { StatusCodes } from "http-status-codes";
import { taskEither } from "fp-ts";
import { QueryInput } from "aws-sdk/clients/dynamodb";

const logPrefix = "Get Transaction by ExternalTxn Transaction Id";

export interface IGetTxnByExternalTxnTxnIdArgs {
  committeeId: string;
  externalTxnTransactionId: string;
}

export const externalTxnTxnExistsUnsafe =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async ({
    committeeId,
    externalTxnTransactionId,
  }: IGetTxnByExternalTxnTxnIdArgs): Promise<boolean> => {
    const query: QueryInput = {
      TableName: txnsTableName,
      IndexName: "TransactionsByExternalTxnTransactionId",
      KeyConditionExpression:
        "committeeId = :committeeId AND externalTxnTransactionId = :externalTxnTransactionId",
      ExpressionAttributeValues: {
        ":committeeId": { S: committeeId },
        ":externalTxnTransactionId": { S: externalTxnTransactionId },
      },
    };

    const res = await dynamoDB.query(query).promise();

    const marshalledRes = res.Items.map((item) => {
      return DynamoDB.Converter.unmarshall(item);
    });

    return marshalledRes.length > 0;
  };

export const externalTxnTxnExists =
  (txnsTable: string) =>
  (dynamoDB: DynamoDB) =>
  (
    args: IGetTxnByExternalTxnTxnIdArgs
  ): TaskEither<ApplicationError, boolean> =>
    pipe(
      tryCatch<ApplicationError, boolean>(
        () => externalTxnTxnExistsUnsafe(txnsTable)(dynamoDB)(args),
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
  (externalTxnTransactionId: string): TaskEither<ApplicationError, boolean> =>
    pipe(
      externalTxnTxnExists(txnsTableName)(dynamoDB)({
        committeeId,
        externalTxnTransactionId,
      }),
      taskEither.map((res) => !res)
    );
