import * as t from "io-ts";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "./application-error";
import { pipe } from "fp-ts/function";
import { taskEither as te } from "fp-ts";
import { decodeError } from "./decode-error.util";

export const decodeRawData =
  (prefix: string) =>
  <T>(type: t.Type<T>) =>
  (res: unknown): TaskEither<ApplicationError, T> => {
    return pipe(
      te.fromEither(type.decode(res)),
      te.mapLeft(decodeError(prefix))
    );
  };
