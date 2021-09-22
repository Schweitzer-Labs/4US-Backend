import { EntityType } from "./enums/entity-type.enum";
import { ValidationError } from "apollo-server-lambda";
import { PaymentMethod } from "./enums/payment-method.enum";
import { CreateContributionInput } from "../graphql/input-types/create-contribution.input-type";
import { AmendContributionInput } from "../graphql/input-types/amend-contrib.input-type";

export const validateContribOrThrow = (
  input: CreateContributionInput | AmendContributionInput
) => {
  const { paymentMethod, entityType, entityName, paymentDate, checkNumber } =
    input;

  if (
    entityType &&
    ![EntityType.Ind, EntityType.Fam, EntityType.Can].includes(entityType) &&
    !entityName
  ) {
    throw new ValidationError(
      "Entity name must be provided for non-individual and non-family contributions"
    );
  }

  if (paymentMethod === PaymentMethod.Check) {
    if (!checkNumber)
      throw new ValidationError(
        "Check number must be provided for contributions by check"
      );
  }

  if ([PaymentMethod.Check, PaymentMethod.Ach].includes(paymentMethod)) {
    if (!paymentDate)
      throw new ValidationError(
        "Payment date must be provided for contributions by check or ACH"
      );
  }

  return true;
};
