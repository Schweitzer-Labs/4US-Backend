import { DynamoDB } from "aws-sdk";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../../application-error";
import { pipe } from "fp-ts/function";
import { StatusCodes } from "http-status-codes";
import { taskEither } from "fp-ts";
import { QueryInput } from "aws-sdk/clients/dynamodb";

const logPrefix = "Get Transaction by ActBlue Transaction Id";

export interface IGetTxnByActBlueTxnIdArgs {
  committeeId: string;
  actBlueTransactionId: string;
}

export const actBlueTxnExistsUnsafe =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async ({
    committeeId,
    actBlueTransactionId,
  }: IGetTxnByActBlueTxnIdArgs): Promise<boolean> => {
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
  (txnsTable: string) =>
  (dynamoDB: DynamoDB) =>
  (args: IGetTxnByActBlueTxnIdArgs): TaskEither<ApplicationError, boolean> =>
    pipe(
      tryCatch<ApplicationError, boolean>(
        () => actBlueTxnExistsUnsafe(txnsTable)(dynamoDB)(args),
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
  (committeeId: string) =>
  (actBlueTransactionId: string): TaskEither<ApplicationError, boolean> =>
    pipe(
      actBlueTxnExists(txnsTableName)(dynamoDB)({
        committeeId,
        actBlueTransactionId,
      }),
      taskEither.map((res) => !res)
    );
