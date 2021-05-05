import * as Joi from "joi";
import { APIGatewayProxyEvent } from "aws-lambda";
import { StatusCodes } from "http-status-codes";
import { TaskEither, left, right } from "fp-ts/TaskEither";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { Contribution } from "./contribution-to-payment";
import { ApplicationError } from "../utils/application-error";
const contribSchema = Joi.object({
  cardNumber: Joi.string().required(),
  cardExpirationMonth: Joi.number().required(),
  cardExpirationYear: Joi.number().required(),
  cardCVC: Joi.string().required(),
  amount: Joi.number().required(),
  stripeUserId: Joi.string().required(),
});

export const eventToObject = (
  event: APIGatewayProxyEvent
): TaskEither<ApplicationError, object> => {
  try {
    return right(JSON.parse(event.body));
  } catch (e) {
    return left(
      new ApplicationError(
        "Error parsing body",
        StatusCodes.UNPROCESSABLE_ENTITY
      )
    );
  }
};

export const objectToContribution = (
  body: object
): TaskEither<ApplicationError, Contribution> => {
  const res = contribSchema.validate(body, { allowUnknown: true });
  if (res.error) {
    console.log("Validation failed", res.error);
    return left(
      new ApplicationError(res.error.message, StatusCodes.BAD_REQUEST)
    );
  } else {
    console.log("Validation succeeded");
    return right(res.value);
  }
};

export const eventToContribution = (
  event: any
): TaskEither<ApplicationError, Contribution> =>
  pipe(
    taskEither.of<ApplicationError, APIGatewayProxyEvent>(event),
    taskEither.chain(eventToObject),
    taskEither.chain(objectToContribution)
  );
