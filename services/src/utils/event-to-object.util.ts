import { left, right, TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "./application-error";
import { StatusCodes } from "http-status-codes";

export const eventToObject = (
  event: any
): TaskEither<ApplicationError, object> => {
  try {
    return right(JSON.parse(event.body));
  } catch (e) {
    return left(
      new ApplicationError(
        "Error parsing body",
        {},
        StatusCodes.UNPROCESSABLE_ENTITY
      )
    );
  }
};
