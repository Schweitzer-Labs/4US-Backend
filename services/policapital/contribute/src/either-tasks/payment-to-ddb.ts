import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { StatusCodes } from "http-status-codes";
import { v4 as uuidv4 } from "uuid";
import { Payment } from "./contribution-to-payment";
import { DynamoDB } from "aws-sdk";

const savePayment =
  (contributionsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (payment: Payment): Promise<any> => {
    const payload = DynamoDB.Converter.marshall({ id: uuidv4(), ...payment });
    return await dynamoDB
      .putItem({
        TableName: contributionsTableName,
        Item: payload,
      })
      .promise();
  };

export const paymentToDDB =
  (contributionsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (payment: Payment): TaskEither<ApplicationError, any> =>
    tryCatch<ApplicationError, any>(
      () => savePayment(contributionsTableName)(dynamoDB)(payment),
      (error) =>
        new ApplicationError("Payment failed", StatusCodes.UNAUTHORIZED, error)
    );
