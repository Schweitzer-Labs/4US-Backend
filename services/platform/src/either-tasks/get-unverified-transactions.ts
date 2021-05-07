import { Client } from "elasticsearch";
import { left, TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";

export const getUnverifiedTransactions = (esClient: Client) => (
  committee: any
): TaskEither<ApplicationError, any> => {
  return left(new ApplicationError("not yet implemented"));
};
