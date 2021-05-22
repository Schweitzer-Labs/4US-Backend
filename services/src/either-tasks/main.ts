import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";
import { contributionToPayment } from "./contribution-to-payment";
import { Stripe } from "stripe";
import { eventToContribution } from "./event-to-contribution";
import { ApplicationError } from "../utils/application-error";
import { successResponse } from "../utils/success-response";
import { DynamoDB } from "aws-sdk";
import { paymentToDDB } from "./payment-to-ddb";

export const main =
  (transactionsTableName: string) =>
  (stripe: Stripe) =>
  (dynamoDB: DynamoDB) =>
  (event: any) =>
    pipe(
      taskEither.of<ApplicationError, any>(event),
      taskEither.chain(eventToContribution),
      taskEither.chain(contributionToPayment(stripe)),
      taskEither.chain(paymentToDDB(transactionsTableName)(dynamoDB)),
      taskEither.fold(
        (error) => task.of(error.toResponse()),
        (result) => task.of(successResponse)
      )
    );
