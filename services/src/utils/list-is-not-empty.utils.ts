import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "./application-error";
import { taskEither } from "fp-ts";

export const listIsNotEmpty = <a>(
  items: a[]
): TaskEither<ApplicationError, a[]> =>
  items.length > 0
    ? taskEither.right(items)
    : taskEither.left(new ApplicationError("List is empty", {}));
