import { CreateContributionInput } from "../input-types/create-contribution.input-type";
import { EntityType } from "../../utils/enums/entity-type.enum";
import { ValidationError } from "apollo-server-lambda";
import { AmendContributionInput } from "../input-types/amend-contrib.input-type";
import { TaskEither } from "fp-ts/TaskEither";
import { taskEither as te } from "fp-ts";
import { Owner } from "../input-types/owner.input-type";

export const validateNYContrib = (
  cInput: CreateContributionInput | AmendContributionInput
): TaskEither<ValidationError, boolean> => {
  switch (cInput.entityType) {
    case EntityType.Llc:
      return validateOwners(cInput.owners)
        ? te.right(true)
        : te.left(
            new ValidationError(
              "NY LLC validation failed: A list of owners with ownership percentages adding up to 100 is required."
            )
          );

    default:
      return te.right(true);
  }
};

const validateOwners = (owners: Owner[]): boolean => {
  if (!owners) return false;
  if (owners.length === 0) return false;
  let total = 0;
  let noNaN = true;
  for (const owner of owners) {
    const maybeNum = Number(owner.percentOwnership);
    if (isNaN(maybeNum)) noNaN = false;
    total = maybeNum + total;
  }

  return total === 100 && noNaN;
};
