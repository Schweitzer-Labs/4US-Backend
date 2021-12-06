import { DynamoDB } from "aws-sdk";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../../application-error";
import { pipe } from "fp-ts/function";
import { StatusCodes } from "http-status-codes";
import { taskEither } from "fp-ts";
import { QueryInput } from "aws-sdk/clients/dynamodb";
import { ITransaction, Transactions } from "../../../model/transaction.type";
import { decodeRawData } from "../../decode-raw-data.util";
import { TransactionType } from "../../enums/transaction-type.enum";

const logPrefix = "Get Transaction by External Transaction Id";

export interface IGetTxnByExternalTxnIdArgs {
  committeeId: string;
  externalTransactionId: string;
}

export const getFeeTxnByExternalId =
  (txnsTable: string) =>
  (ddb: DynamoDB) =>
  (committeeId: string) =>
  (externalTransactionId: string): TaskEither<ApplicationError, ITransaction> =>
    pipe(
      getTxnsByExternalId(txnsTable)(ddb)({
        committeeId,
        externalTransactionId,
      }),
      taskEither.chain(txnsToFeeTxn)
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

const getTxnsByExternalIdUnsafe =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async ({
    committeeId,
    externalTransactionId,
  }: IGetTxnByExternalTxnIdArgs): Promise<unknown[]> => {
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

    return res.Items.map((item) => {
      return DynamoDB.Converter.unmarshall(item);
    });
  };

const txnsToFeeTxn = (
  txns: ITransaction[]
): TaskEither<ApplicationError, ITransaction> => {
  const contribList = txns.filter(
    (txn) => txn.transactionType === TransactionType.Contribution
  );
  const feeList = txns.filter(
    (txn) =>
      txn.transactionType === TransactionType.Disbursement &&
      !!txn.feeForTxn &&
      txn.feeForTxn === contribList[0]?.id
  );
  switch (feeList.length) {
    case 0:
      return taskEither.left(
        new ApplicationError("No fee was found for external contribution", {})
      );
    case 1:
      return taskEither.right(feeList[0]);
    default:
      return taskEither.left(
        new ApplicationError(
          "Multiple fees were found for external contribution",
          {}
        )
      );
  }
};

const getTxnsByExternalId =
  (txnsTable: string) =>
  (ddb: DynamoDB) =>
  (
    args: IGetTxnByExternalTxnIdArgs
  ): TaskEither<ApplicationError, ITransaction[]> =>
    pipe(
      getRawTxnsByExternalId(txnsTable)(ddb)(args),
      taskEither.chain(decodeRawData("External Contribution")(Transactions))
    );

const getRawTxnsByExternalId =
  (txnsTable: string) =>
  (dynamoDB: DynamoDB) =>
  (args: IGetTxnByExternalTxnIdArgs): TaskEither<ApplicationError, unknown[]> =>
    tryCatch(
      () => getTxnsByExternalIdUnsafe(txnsTable)(dynamoDB)(args),
      (e) =>
        new ApplicationError(
          "Get transactions by External ID request failed",
          e,
          StatusCodes.INTERNAL_SERVER_ERROR
        )
    );

const externalTxnExists =
  (txnsTable: string) =>
  (dynamoDB: DynamoDB) =>
  (args: IGetTxnByExternalTxnIdArgs): TaskEither<ApplicationError, boolean> =>
    pipe(
      getRawTxnsByExternalId(txnsTable)(dynamoDB)(args),
      taskEither.chain((res) =>
        res.length > 0 ? taskEither.right(true) : taskEither.right(false)
      )
    );
