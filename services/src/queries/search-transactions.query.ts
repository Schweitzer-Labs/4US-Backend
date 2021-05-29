import { DynamoDB } from "aws-sdk";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { pipe } from "fp-ts/function";
import { StatusCodes } from "http-status-codes";
import { taskEither } from "fp-ts";
import {
  toExpressionAttributeValueBool,
  toExpressionAttributeValueString,
  toFilterExpression,
  validateDDBResponse,
} from "../repositories/ddb.utils";
import { ITransaction, Transactions } from "./search-transactions.decoder";
import { TransactionsArg } from "../args/transactions.arg";

const getTransactionsRes =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  ({
    committeeId,
    transactionType,
    bankVerified,
    ruleVerified,
  }: TransactionsArg) =>
  async (): Promise<any> => {
    const filterExpressionString = [
      ...toFilterExpression("transactionType", transactionType),
      ...toFilterExpression("bankVerified", bankVerified),
      ...toFilterExpression("ruleVerified", ruleVerified),
    ].join(" AND ");

    const FilterExpression =
      filterExpressionString === ""
        ? {}
        : { FilterExpression: filterExpressionString };

    const res = await dynamoDB
      .query({
        TableName: txnsTableName,
        KeyConditionExpression: "committeeId = :committeeId",
        ...FilterExpression,
        ExpressionAttributeValues: {
          ":committeeId": { S: committeeId },
          ...toExpressionAttributeValueString(
            "transactionType",
            transactionType
          ),
          ...toExpressionAttributeValueBool("bankVerified", bankVerified),
          ...toExpressionAttributeValueBool("ruleVerified", ruleVerified),
        },
      })
      .promise();
    return res.Items.map((item) => DynamoDB.Converter.unmarshall(item));
  };

export const searchTransactions =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (
    transactionsArg: TransactionsArg
  ): TaskEither<ApplicationError, ITransaction[]> =>
    pipe(
      tryCatch<ApplicationError, any>(
        () => getTransactionsRes(txnsTableName)(dynamoDB)(transactionsArg)(),
        (e) =>
          new ApplicationError(
            "Get transaction request failed",
            e,
            StatusCodes.INTERNAL_SERVER_ERROR
          )
      ),
      taskEither.chain(validateDDBResponse(Transactions))
    );
