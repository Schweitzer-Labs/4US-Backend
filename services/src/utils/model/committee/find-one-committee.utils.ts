import { ICommittee } from "../../../model/committee.type";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../application-error";
import { taskEither } from "fp-ts";

export const findOne = (
  list: ICommittee[]
): TaskEither<ApplicationError, ICommittee> => {
  if (list.length > 0) {
    // @ToDo Address case with multiple committees with same stripe account.
    return taskEither.right(list[0]);
  } else {
    return taskEither.left(new ApplicationError("Does not exist", {}));
  }
};
