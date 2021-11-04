import { EntityType } from "../../utils/enums/entity-type.enum";
import { PaymentMethod } from "../../utils/enums/payment-method.enum";
import { validateNYContrib } from "./ny.validators";
import { TaskEither } from "fp-ts/TaskEither";
import { taskEither as te } from "fp-ts";
import { teToRightOrThrow } from "../../utils/te-to-right-or-throw.util";
import { ApplicationError } from "../../utils/application-error";
import { ValidationError } from "apollo-server-lambda";
import { ContribInput } from "../input-types/contrib-input.input-type";
import { pipe } from "fp-ts/function";
import { ICommittee } from "../../model/committee.type";

export const validateContribOrThrowGQLError =
  (com: ICommittee) =>
  (input: ContribInput): Promise<boolean> => {
    return teToRightOrThrow(validateContribOrGQLError(com)(input));
  };

const validateContribOrGQLError =
  (com: ICommittee) =>
  (input: ContribInput): TaskEither<ValidationError, boolean> =>
    pipe(
      validateContrib(com)(input),
      te.mapLeft((appErr) => new ValidationError(appErr.message))
    );

export const validateContrib =
  (com: ICommittee) =>
  (input: ContribInput): TaskEither<ApplicationError, boolean> => {
    const { paymentMethod, entityType, entityName, paymentDate, checkNumber } =
      input;

    if (
      entityType &&
      ![EntityType.Ind, EntityType.Fam, EntityType.Can].includes(entityType) &&
      !entityName
    ) {
      return te.left(
        new ApplicationError(
          "Entity name must be provided for non-individual and non-family contributions",
          input
        )
      );
    }

    if (paymentMethod === PaymentMethod.Check) {
      if (!checkNumber)
        return te.left(
          new ApplicationError(
            "Check number must be provided for contributions by check",
            input
          )
        );
    }

    if ([PaymentMethod.Check, PaymentMethod.Ach].includes(paymentMethod)) {
      if (!paymentDate)
        return te.left(
          new ApplicationError(
            "Payment date must be provided for contributions by check or ACH",
            input
          )
        );
    }

    // Jurisdiction specific validation
    switch (com.state) {
      // @ToDo Committee state data is lowercase while all other state data is uppercase
      case "ny":
        return validateNYContrib(input);
      default:
        return te.right(true);
    }
  };
