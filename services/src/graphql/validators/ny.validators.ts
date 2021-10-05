import { EntityType } from "../../utils/enums/entity-type.enum";
import { TaskEither } from "fp-ts/TaskEither";
import { taskEither as te } from "fp-ts";
import { Owner } from "../input-types/owner.input-type";
import { ApplicationError } from "../../utils/application-error";
import { ContribInput } from "../input-types/contrib-input.input-type";

export const validateNYContrib = (
  cInput: ContribInput
): TaskEither<ApplicationError, boolean> => {
  switch (cInput.entityType) {
    case EntityType.Llc:
      return validateOwners(cInput.owners)
        ? te.right(true)
        : te.left(
            new ApplicationError(
              "NY LLC validation failed: A list of owners with ownership percentages adding up to 100 is required.",
              cInput
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
