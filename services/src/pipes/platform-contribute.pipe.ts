import { flow, pipe } from "fp-ts/function";
import { chain, left, right, TaskEither } from "fp-ts/TaskEither";
import Joi from "joi";
import { plainToClass } from "class-transformer";
import { StatusCodes } from "http-status-codes";
import { EntityType, entityTypes } from "../utils/enums/entity-type.enum";
import {
  EmploymentStatus,
  employmentStatuses,
} from "../utils/enums/employment-status";
import { CreateContributionInput } from "../input-types/create-contribution.input-type";
import { ApplicationError } from "../utils/application-error";
import { DynamoDB } from "aws-sdk";
import { Stripe } from "stripe";
import { IInstantIdConfig } from "../clients/lexis-nexis/lexis-nexis.client";
import { ITransaction } from "../queries/search-transactions.decoder";
import { taskEither as te } from "fp-ts";
import { getCommitteeById } from "../queries/get-committee-by-id.query";
import { runRulesAndProcess } from "./run-rules-and-process.pipe";
import { ANONYMOUS } from "../utils/tokens/users.token";

const stringOpt = (min = 1, max = 200) => Joi.string().min(min).max(max);
const stringReq = (min = 1, max = 200) =>
  Joi.string().min(min).max(max).required();

const schema = Joi.object({
  committeeId: stringReq(2),
  amount: Joi.number().min(50).max(200000000).required(),
  firstName: stringReq(),
  lastName: stringReq(),
  addressLine1: stringReq(),
  city: stringReq(),
  state: stringReq(2, 2),
  postalCode: stringReq(5, 10),
  entityType: Joi.string().valid(entityTypes),
  emailAddress: Joi.string().email(),
  cardNumber: stringReq(),
  cardExpirationMonth: Joi.number().min(1).max(12).required(),
  cardExpirationYear: Joi.number().min(2021).max(2050).required(),
  cardCVC: stringReq(2, 4),
  entityName: stringOpt(),
  employer: stringOpt(),
  employmentStatus: Joi.string().valid(employmentStatuses),
  occupation: stringOpt(),
  middleName: stringOpt(),
  refCode: stringOpt(),
  addressLine2: stringOpt(),
  phoneNumber: stringOpt(5, 15),
  attestsToBeingAnAdultCitizen: Joi.bool(),
});

const validateNonInd = (
  contrib: CreateContributionInput
): TaskEither<ApplicationError, CreateContributionInput> => {
  const { entityType, entityName } = contrib;

  if (![EntityType.Ind, EntityType.Fam].includes(entityType) && !entityName) {
    return left(
      new ApplicationError(
        "Entity name must be provided for non-individual and non-family contributions",
        {},
        StatusCodes.BAD_REQUEST
      )
    );
  }
  return right(contrib);
};

const validateInd = (
  c: CreateContributionInput
): TaskEither<ApplicationError, CreateContributionInput> => {
  if ([EntityType.Ind, EntityType.Fam].includes(c.entityType)) {
    if (!c.employmentStatus) {
      return left(
        new ApplicationError(
          "Employment status of donor must be provided.",
          {},
          StatusCodes.BAD_REQUEST
        )
      );
    }
    if (
      [EmploymentStatus.Employed, EmploymentStatus.SelfEmployed].includes(
        c.employmentStatus
      ) &&
      !c.employer
    ) {
      return left(
        new ApplicationError(
          "Employer of donor must be provided.",
          {},
          StatusCodes.BAD_REQUEST
        )
      );
    }
  }
  return right(c);
};

const eventToCreateContribInput = (
  event: any
): TaskEither<ApplicationError, CreateContributionInput> => {
  const res = schema.validate(event.body, { allowUnknown: true });
  if (res.error) {
    return left(
      new ApplicationError(res.error.message, {}, StatusCodes.BAD_REQUEST)
    );
  } else {
    //@Todo Make this a typesafe operation
    const contrib = plainToClass(CreateContributionInput, res.value);
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
  (instantIdConfig: IInstantIdConfig) =>
  (event: any): TaskEither<ApplicationError, ITransaction> => {
    return pipe(
      eventToCreateContribInput(event),
      te.chain(validateInd),
      te.chain(validateNonInd),
      te.chain((contrib) =>
        pipe(
          getCommitteeById(committeesTableName)(dynamoDB)(contrib.committeeId),
          te.chain((committee) =>
            pipe(
              runRulesAndProcess(billableEventsTableName)(donorsTableName)(
                txnsTableName
              )(rulesTableName)(dynamoDB)(stripe)(instantIdConfig)(ANONYMOUS)(
                committee
              )(contrib)
            )
          )
        )
      )
    );
  };