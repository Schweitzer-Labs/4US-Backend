import { Finicity } from "../clients/finicity/finicity.client";
import { left, TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";

export const getFinicityTransactions = (finicity: Finicity) => (
  committee: any
): TaskEither<ApplicationError, any> => {
  return left(new ApplicationError("not yet implemented"));
};
