import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "./application-error";
import { taskEither } from "fp-ts";
import { unknown } from "io-ts";

export const strToJSON = (str: string): TaskEither<ApplicationError, unknown> =>
  taskEither.tryCatch(
    () => strToJSONPromise(str),
    (err) => new ApplicationError("JSON parse failed", err)
  );

const strToJSONPromise = async (str: string): Promise<unknown> =>
  await JSON.parse(str);
