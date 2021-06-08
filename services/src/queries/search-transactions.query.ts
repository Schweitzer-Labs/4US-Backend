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
import { Order } from "../utils/enums/order.enum";

const getTransactionsRes =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  ({
    committeeId,
    transactionType,
    bankVerified,
    ruleVerified,
    order = Order.Desc,
    donorId,
    entityType,
  }: TransactionsArg) =>
  async (): Promise<object[]> => {
    const byDonorIndex = donorId
      ? { IndexName: "TransactionsByCommitteeDonorIndex" }
      : {};

    const filterExpressionString = [
      ...toFilterExpression("transactionType", transactionType),
      ...toFilterExpression("bankVerified", bankVerified),
      ...toFilterExpression("ruleVerified", ruleVerified),
      ...toFilterExpression("entityType", entityType),
    ].join(" AND ");

    const FilterExpression =
      filterExpressionString === ""
        ? {}
        : { FilterExpression: filterExpressionString };

    const keyConditionExpress = donorId
      ? "committeeId = :committeeId AND donorId = :donorId"
      : "committeeId = :committeeId";
    const query = {
      TableName: txnsTableName,
      ...byDonorIndex,
      KeyConditionExpression: keyConditionExpress,
      ...FilterExpression,
      ScanIndexForward: order === Order.Asc,
      ExpressionAttributeValues: {
        ":committeeId": { S: committeeId },
        ...toExpressionAttributeValueString("transactionType", transactionType),
        ...toExpressionAttributeValueString("donorId", donorId),
        ...toExpressionAttributeValueBool("bankVerified", bankVerified),
        ...toExpressionAttributeValueBool("ruleVerified", ruleVerified),
        ...toExpressionAttributeValueString("entityType", entityType),
      },
    };

    const res = await dynamoDB.query(query).promise();
    const marshalledRes = res.Items.map((item) =>
      DynamoDB.Converter.unmarshall(item)
    );
    console.log(marshalledRes);
    return marshalledRes;
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
            "Get transactions request failed",
            e,
            StatusCodes.INTERNAL_SERVER_ERROR
          )
      ),
      taskEither.chain(validateDDBResponse(Transactions))
    );
