import headers from "./headers";
import { APIGatewayProxyEvent } from "aws-lambda";
import { pipe } from "fp-ts/function";
import { eventToObject, objectToContribution } from "./validators";
import {
  Contribution,
  processPaymentFromContribution,
} from "./process-payment-from-contribution";
import { task, taskEither } from "fp-ts";
import { ApplicationError } from "./application-error";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { StatusCodes } from "http-status-codes";

const eventToContribution = (
  event: APIGatewayProxyEvent
): TaskEither<ApplicationError, Contribution> =>
  pipe(
    taskEither.of<ApplicationError, APIGatewayProxyEvent>(event),
    taskEither.chain(eventToObject),
    taskEither.chain(objectToContribution)
  );

const contributionToPayment = (
  contribution: Contribution
): TaskEither<ApplicationError, object> =>
  tryCatch<ApplicationError, any>(
    () => processPaymentFromContribution(contribution),
    (reason) => new ApplicationError("Payment failed", StatusCodes.UNAUTHORIZED)
  );

const successResponse = (payload) => {
  return {
    statusCode: StatusCodes.OK,
    body: JSON.stringify({
      message: "success",
    }),
    headers,
  };
};

const eventToPayment = (event: APIGatewayProxyEvent) =>
  pipe(
    taskEither.of<ApplicationError, APIGatewayProxyEvent>(event),
    taskEither.chain(eventToContribution),
    taskEither.chain(contributionToPayment),
    taskEither.fold(
      (error) => task.of(error.toResponse()),
      (result) => task.of(successResponse(result))
    )
  );

export default (event: APIGatewayProxyEvent) => eventToPayment(event)();
