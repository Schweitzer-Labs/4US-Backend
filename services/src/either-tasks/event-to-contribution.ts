import * as Joi from "joi";
import { APIGatewayProxyEvent } from "aws-lambda";
import { StatusCodes } from "http-status-codes";
import { TaskEither, left, right } from "fp-ts/TaskEither";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { ApplicationError } from "../utils/application-error";
import { Source } from "../utils/enums/source.enum";
import { eventToObject } from "../utils/event-to-object.util";

export interface IContribution {
  amount: number;
  cardNumber?: string;
  cardExpirationMonth?: number;
  cardExpirationYear?: number;
  cardCVC?: string;
  paymentMethod: string;
  cardNumberLastFourDigits?: string;
  firstName?: string;
  lastName?: string;
  email?: string;
  occupation?: string;
  employer?: string;
  addressLine1?: string;
  addressLine2?: string;
  city?: string;
  state?: string;
  postalCode?: string;
  phoneNumber?: string;
  refCode?: string;
  committeeId: string;
  entityType?: string;
  donorId?: string;
  ruleCode?: string;
  source?: Source;
  createdByUser?: string;
}

const contribSchema = Joi.object({
  cardNumber: Joi.string().required(),
  cardExpirationMonth: Joi.number().required(),
  cardExpirationYear: Joi.number().required(),
  cardCVC: Joi.string().required(),
  amount: Joi.number().required(),
  email: Joi.string().required(),
  firstName: Joi.string().required(),
  lastName: Joi.string().required(),
  addressLine1: Joi.string().required(),
  city: Joi.string().required(),
  state: Joi.string().required(),
  postalCode: Joi.string().required(),
  committee: Joi.string().required(),
  entityType: Joi.string().required(),
  attestsToBeingAdultCitizen: Joi.bool().required(),
});

export const objectToContribution = (
  body: object
): TaskEither<ApplicationError, IContribution> => {
  const res = contribSchema.validate(body, { allowUnknown: true });
  if (res.error) {
    console.log("Validation failed", res.error);
    return left(
      new ApplicationError(res.error.message, {}, StatusCodes.BAD_REQUEST)
    );
  } else {
    console.log("Validation succeeded");
    // @Todo Change email to emailAddress in API.
    const txn = {
      ...res.value,
      committeeId: res.value.committee,
      emailAddress: res.value.email,
    };
    return right(txn);
  }
};

export const eventToContribution = (
  event: any
): TaskEither<ApplicationError, IContribution> =>
  pipe(
    taskEither.of<ApplicationError, APIGatewayProxyEvent>(event),
    taskEither.chain(eventToObject),
    taskEither.chain(objectToContribution)
  );
