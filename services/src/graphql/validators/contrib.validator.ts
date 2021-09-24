import { ICommittee } from "../../queries/get-committee-by-id.query";
import { CreateContributionInput } from "../input-types/create-contribution.input-type";
import { AmendContributionInput } from "../input-types/amend-contrib.input-type";
import { EntityType } from "../../utils/enums/entity-type.enum";
import { ValidationError } from "apollo-server-lambda";
import { PaymentMethod } from "../../utils/enums/payment-method.enum";
import { validateNYContrib } from "./ny.validators";
import { TaskEither } from "fp-ts/TaskEither";
import { taskEither as te } from "fp-ts";
import { teToRightOrThrow } from "../../utils/te-to-right-or-throw.util";

export const validateContribOrThrow =
  (com: ICommittee) =>
  (
    input: CreateContributionInput | AmendContributionInput
  ): Promise<boolean> => {
    return teToRightOrThrow(validateContrib(com)(input));
  };

const validateContrib =
  (com: ICommittee) =>
  (
    input: CreateContributionInput | AmendContributionInput
  ): TaskEither<ValidationError, boolean> => {
    const { paymentMethod, entityType, entityName, paymentDate, checkNumber } =
      input;

    if (
      entityType &&
      ![EntityType.Ind, EntityType.Fam, EntityType.Can].includes(entityType) &&
      !entityName
    ) {
      te.left(
        new ValidationError(
          "Entity name must be provided for non-individual and non-family contributions"
        )
      );
    }

    if (paymentMethod === PaymentMethod.Check) {
      if (!checkNumber)
        te.left(
          new ValidationError(
            "Check number must be provided for contributions by check"
          )
        );
    }

    if ([PaymentMethod.Check, PaymentMethod.Ach].includes(paymentMethod)) {
      if (!paymentDate)
        te.left(
          new ValidationError(
            "Payment date must be provided for contributions by check or ACH"
          )
        );
    }

    // Jurisdiction specific validation
    switch (com.state) {
      // @ToDo Committee state data is lowercase while all other state data is uppercase
      case "ny":
        return validateNYContrib(input);
      default:
        return te.of(true);
    }
  };
