import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "./application-error";
import { taskEither } from "fp-ts";
import { StatusCodes } from "http-status-codes";

export const isEmpty =
  (logPrefix: string) =>
  (id: string) =>
  (val: any): TaskEither<ApplicationError, any> =>
    Object.keys(val)?.length > 0
      ? taskEither.right(val)
      : taskEither.left(
          new ApplicationError(
            `${logPrefix}: Invalid ID ${id}`,
            {},
            StatusCodes.NOT_FOUND
          )
        );
