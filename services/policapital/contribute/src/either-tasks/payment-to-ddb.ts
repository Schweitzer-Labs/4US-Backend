import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { StatusCodes } from "http-status-codes";
import { v4 as uuidv4 } from "uuid";
import { Payment } from "./contribution-to-payment";
import { DynamoDB } from "aws-sdk";
import {
  boolToDDBBool,
  numberToDDBNumber,
  stringToDDBString,
} from "../utils/ddb.utils";

const savePayment =
  (env: string) =>
  (dynamoDB: DynamoDB) =>
  async (payment: Payment): Promise<any> => {
    return await dynamoDB
      .putItem({
        TableName: `contribution-transactions-${env}`,
        Item: encodePayment(payment),
      })
      .promise();
  };

export const paymentToDDB =
  (env: string) =>
  (dynamoDB: DynamoDB) =>
  (payment: Payment): TaskEither<ApplicationError, any> =>
    tryCatch<ApplicationError, any>(
      () => savePayment(env)(dynamoDB)(payment),
      (error) =>
        new ApplicationError("Payment failed", StatusCodes.UNAUTHORIZED, error)
    );

const encodePayment = (payment: Payment): any => ({
  ...stringToDDBString("id", uuidv4()),
  ...stringToDDBString("committee", payment.committee),
  ...numberToDDBNumber("amount", payment.amount),
  ...boolToDDBBool(
    "attestsToBeingAdultCitizen",
    payment.attestsToBeingAdultCitizen || false
  ),
  ...stringToDDBString("firstName", payment.firstName),
  ...stringToDDBString("lastName", payment.lastName),
  ...stringToDDBString("employer", payment.employer),
  ...stringToDDBString("occupation", payment.occupation),
  ...stringToDDBString("email", payment.email),
  ...stringToDDBString("addressLine1", payment.addressLine1),
  ...stringToDDBString("addressLine2", payment.addressLine2),
  ...stringToDDBString("city", payment.city),
  ...stringToDDBString("state", payment.state),
  ...stringToDDBString("postalCode", payment.postalCode),
  ...stringToDDBString("phoneNumber", payment.phoneNumber),
  ...stringToDDBString("refCode", payment.refCode),
  ...stringToDDBString("paymentMethod", payment.paymentMethod),
  ...stringToDDBString("contributorType", payment.contributorType),
  ...stringToDDBString(
    "cardNumberLastFourDigits",
    payment.cardNumberLastFourDigits
  ),
});
