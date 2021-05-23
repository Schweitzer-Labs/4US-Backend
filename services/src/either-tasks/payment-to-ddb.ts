import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { StatusCodes } from "http-status-codes";
import { v4 as uuidv4 } from "uuid";
import { Payment } from "./contribution-to-payment";
import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../queries/search-transactions.decoder";
import { now } from "../utils/time.utils";

const savePayment =
  (transactionsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (payment: Payment): Promise<any> => {
    const transaction: ITransaction = {
      ...payment,
      id: uuidv4(),
      committeeId: payment.committee,
      direction: "in",
      bankVerified: false,
      ruleVerified: false,
      initiatedTimestamp: now(),
      transactionType: "contribution",
    };

    console.log(
      `Writing contribution to ${transactionsTableName}`,
      transaction
    );

    const payload = DynamoDB.Converter.marshall(transaction);
    return await dynamoDB
      .putItem({
        TableName: transactionsTableName,
        Item: payload,
      })
      .promise();
  };

export const paymentToDDB =
  (transactionsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (payment: Payment): TaskEither<ApplicationError, any> =>
    tryCatch<ApplicationError, any>(
      () => savePayment(transactionsTableName)(dynamoDB)(payment),
      (error) =>
        new ApplicationError(
          "DDB Write failed",
          error,
          StatusCodes.UNAUTHORIZED
        )
    );