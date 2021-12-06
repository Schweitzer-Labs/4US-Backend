import { pipe } from "fp-ts/function";
import { listIsNotEmpty } from "./list-is-not-empty.utils";
import { taskEither } from "fp-ts";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "./application-error";

export const getOneFromList = <a>(
  items: a[]
): TaskEither<ApplicationError, a> =>
  pipe(
    listIsNotEmpty<a>(items),
    taskEither.chain(() => taskEither.of(items[0]))
  );
