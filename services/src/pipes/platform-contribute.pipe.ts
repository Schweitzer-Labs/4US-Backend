import { pipe } from "fp-ts/function";
import { left, right, TaskEither } from "fp-ts/TaskEither";
import Joi from "joi";
import { plainToClass } from "class-transformer";
import { StatusCodes } from "http-status-codes";
import { EntityType, entityTypes } from "../utils/enums/entity-type.enum";
import {
  EmploymentStatus,
  employmentStatuses,
} from "../utils/enums/employment-status";
import { CreateContributionInput } from "../graphql/input-types/create-contribution.input-type";
import { ApplicationError } from "../utils/application-error";
import { DynamoDB } from "aws-sdk";
import { Stripe } from "stripe";
import { ITransaction } from "../model/transaction.type";
import { taskEither as te } from "fp-ts";
import { getCommitteeById } from "../utils/model/get-committee-by-id.query";
import { runRulesAndProcess } from "./run-rules-and-process.pipe";
import { ANONYMOUS } from "../utils/tokens/users.token";
import { eventToObject } from "../utils/event-to-object.util";
import { PaymentMethod } from "../utils/enums/payment-method.enum";
import { ILexisNexisConfig } from "../clients/lexis-nexis/lexis-nexis.client";
import { enumToValues } from "../utils/enums/poly.util";
import { State } from "../utils/enums/state.enum";
import { validateMAContrib } from "../graphql/validators/ma.validators";
import { validateNYContrib } from "../graphql/validators/ny.validators";
import { stringOpt, stringReq } from "../utils/joi.utils";
import { ICommittee } from "../model/committee.type";

const ownerSchema = {
  firstName: Joi.string(),
  lastName: Joi.string(),
  addressLine1: Joi.string(),
  addressLine2: Joi.any().optional(),
  city: Joi.string(),
  state: Joi.string(),
  postalCode: Joi.string(),
  percentOwnership: Joi.string(),
};

const schema = Joi.object({
  committeeId: stringReq(2),
  amount: Joi.number().min(50).max(200000000).required(),
  firstName: stringReq(),
  lastName: stringReq(),
  addressLine1: stringReq(),
  city: stringReq(),
  state: Joi.string()
    .valid(...enumToValues(State))
    .required(),
  postalCode: stringReq(5, 10),
  entityType: Joi.string()
    .valid(...entityTypes)
    .required(),
  emailAddress: Joi.string().email(),
  cardNumber: stringReq(),
  cardExpirationMonth: Joi.number().min(1).max(12).required(),
  cardExpirationYear: Joi.number().min(2021).max(2050).required(),
  cardCVC: stringReq(2, 4),
  entityName: stringOpt(),
  employer: stringOpt(),
  employmentStatus: Joi.string().valid(...employmentStatuses),
  occupation: stringOpt(),
  middleName: stringOpt(),
  refCode: stringOpt(),
  addressLine2: stringOpt(),
  phoneNumber: stringOpt(5, 15),
  attestsToBeingAnAdultCitizen: Joi.bool(),
  owners: Joi.array().items(Joi.object(ownerSchema)),
});

const validateNonInd = (
  contrib: CreateContributionInput
): TaskEither<ApplicationError, boolean> => {
  const { entityType, entityName } = contrib;

  if (
    ![EntityType.Ind, EntityType.Fam, EntityType.Can].includes(entityType) &&
    !entityName
  ) {
    return left(
      new ApplicationError(
        "Entity name must be provided for non-individual and non-family contributions",
        {},
        StatusCodes.BAD_REQUEST
      )
    );
  }
  return right(true);
};

// @ToDo generalize function
const eventToCreateContribInput = (
  event: any
): TaskEither<ApplicationError, CreateContributionInput> => {
  const res = schema.validate(event);
  if (res.error) {
    return left(
      new ApplicationError(res.error.message, {}, StatusCodes.BAD_REQUEST)
    );
  } else {
    //@Todo Make this a typesafe operation
    const contrib = plainToClass(CreateContributionInput, {
      ...res.value,
      paymentMethod: PaymentMethod.Credit,
    });
    return right(contrib);
  }
};

export const platformContribute =
  (billableEventsTableName: string) =>
  (donorsTableName: string) =>
  (committeesTableName: string) =>
  (txnsTableName: string) =>
  (rulesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (stripe: Stripe) =>
  (lnConfig: ILexisNexisConfig) =>
  (event: any): TaskEither<ApplicationError, ITransaction> => {
    console.log("Platform contribute pipe initiated", JSON.stringify(event));
    return pipe(
      eventToObject(event),
      te.chain(eventToCreateContribInput),
      te.map((c) => ({ ...c, processPayment: true })),
      te.chain((contrib) =>
        pipe(
          validateNonInd(contrib),
          te.chain(() =>
            getCommitteeById(committeesTableName)(dynamoDB)(contrib.committeeId)
          ),
          te.chain((committee: ICommittee) =>
            pipe(
              validateMAContrib(committee)(contrib),
              te.chain(() => validateNYContrib(contrib)),
              te.chain(() =>
                runRulesAndProcess(billableEventsTableName)(donorsTableName)(
                  txnsTableName
                )(rulesTableName)(dynamoDB)(stripe)(lnConfig)(ANONYMOUS)(
                  committee
                )(contrib)
              )
            )
          )
        )
      )
    );
  };
