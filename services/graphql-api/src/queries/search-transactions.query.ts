import { DynamoDB } from "aws-sdk";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { pipe } from "fp-ts/function";
import { StatusCodes } from "http-status-codes";
import { taskEither } from "fp-ts";
import { validateDDBResponse } from "../repositories/ddb.utils";
import {
  ddbResponseToTransactions,
  DDBTransactionsRes,
  ITransaction,
} from "./search-transactions.decoder";

const getTransactionsRes =
  (env = "dev") =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string) =>
  async (): Promise<any> => {
    const transactionsTable = `transactions-${env}`;

    const query = `SELECT * FROM "${transactionsTable}" WHERE committeeId='${committeeId}'`;
    const params = {};
    const otherRes = await dynamoDB
      .query({
        TableName: transactionsTable,
        KeyConditionExpression: "committeeId = ':committeeId'",
        // FilterExpression: "",
        ExpressionAttributeValues: {
          committeeId: { S: committeeId },
          // amount: { N: "100" },
        },
      })
      .promise();

    const res = await dynamoDB
      .executeStatement({
        //@ ToDo make table name configurable.
        Statement: query,
      })
      .promise();

    console.log(otherRes);

    return res;
  };

export const searchTransactions =
  (env: string) =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string): TaskEither<ApplicationError, ITransaction[]> =>
    pipe(
      tryCatch<ApplicationError, any>(
        () => getTransactionsRes(env)(dynamoDB)(committeeId)(),
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
