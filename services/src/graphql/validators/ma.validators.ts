import { ICommittee } from "../../queries/get-committee-by-id.query";
import { CreateContributionInput } from "../input-types/create-contribution.input-type";
import { left, right, TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";
import { EntityType } from "../../utils/enums/entity-type.enum";
import { StatusCodes } from "http-status-codes";
import { EmploymentStatus } from "../../utils/enums/employment-status";
import { State } from "../../utils/enums/state.enum";
import { taskEither } from "fp-ts";
import { ContribInput } from "../input-types/contrib-input.input-type";

export const validateMAContrib =
  (committee: ICommittee) =>
  (c: ContribInput): TaskEither<ApplicationError, boolean> => {
    if (committee.state === State.MA) {
      return validateInd(c);
    } else {
      return taskEither.right(true);
    }
  };

const validateInd = (
  c: ContribInput
): TaskEither<ApplicationError, boolean> => {
  if ([EntityType.Ind, EntityType.Fam, EntityType.Can].includes(c.entityType)) {
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
      !c.employer &&
      !c.occupation
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
  return right(true);
};
