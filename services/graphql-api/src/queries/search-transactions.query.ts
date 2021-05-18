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
import {
  ddbResponseToTransactions,
  DDBTransactionsRes,
  ITransaction,
} from "./search-transactions.decoder";
import { TransactionsArg } from "../args/transactions.arg";

const getTransactionsRes =
  (env = "dev") =>
  (dynamoDB: DynamoDB) =>
  ({
    committeeId,
    transactionType,
    bankVerified,
    ruleVerified,
  }: TransactionsArg) =>
  async (): Promise<any> => {
    const transactionsTable = `transactions-${env}`;
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
        TableName: transactionsTable,
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

    console.log(res);

    return res;
  };

export const searchTransactions =
  (env: string) =>
  (dynamoDB: DynamoDB) =>
  (
    transactionsArg: TransactionsArg
  ): TaskEither<ApplicationError, ITransaction[]> =>
    pipe(
      tryCatch<ApplicationError, any>(
        () => getTransactionsRes(env)(dynamoDB)(transactionsArg)(),
        (e) =>
          new ApplicationError(
            "Get transaction request failed",
            e,
            StatusCodes.INTERNAL_SERVER_ERROR
          )
      ),
      taskEither.chain(validateDDBResponse(DDBTransactionsRes)),
      taskEither.chain(ddbResponseToTransactions)
    );
