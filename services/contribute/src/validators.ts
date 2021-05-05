import * as Joi from "joi";
import headers from "./headers";
import { APIGatewayEvent, APIGatewayProxyEvent } from "aws-lambda";
import { ApplicationError } from "./application-error";
import { StatusCodes } from "http-status-codes";
import { TaskEither, left, right } from "fp-ts/TaskEither";
import { Contribution } from "./execute-payment";
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
