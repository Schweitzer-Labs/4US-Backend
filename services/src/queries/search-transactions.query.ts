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
import { QueryInput } from "aws-sdk/clients/dynamodb";

const logPrefix = "Get Transactions";

const toExclusiveStartKey = (committeeId: string) => (id?: string) =>
  id
    ? {
        ExclusiveStartKey: {
          id: { S: id },
          committeeId: { S: committeeId },
        },
      }
    : {};

const toLimit = (limit?: number) => (limit ? { Limit: limit } : {});

export const getTransactionsRes =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async ({
    committeeId,
    transactionType,
    bankVerified,
    ruleVerified,
    order = Order.Desc,
    donorId,
    entityType,
    take,
    fromId,
  }: TransactionsArg): Promise<ITransaction[]> => {
    console.log("search txn arg committeeId", committeeId);
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
    const query: QueryInput = {
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
      ...toExclusiveStartKey(committeeId)(fromId),
      ...toLimit(take),
    };

    const res = await dynamoDB.query(query).promise();

    const marshalledRes: any = res.Items.map((item) =>
      DynamoDB.Converter.unmarshall(item)
    );

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
        () => getTransactionsRes(txnsTableName)(dynamoDB)(transactionsArg),
        (e) =>
          new ApplicationError(
            "Get transactions request failed",
            e,
            StatusCodes.INTERNAL_SERVER_ERROR
          )
      ),
      taskEither.chain(validateDDBResponse(logPrefix)(Transactions))
    );
