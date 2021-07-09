import * as t from "io-ts";
import { left, right, TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { pipe } from "fp-ts/function";
import { taskEither as te } from "fp-ts";
import { decodeError } from "../utils/decode-error.util";

export const ddbString = t.type({
  S: t.string,
});

export const ddbStringList = t.type({
  SS: t.array(t.string),
});

export const validateDDBResponse =
  (prefix: string) =>
  <T>(type: t.Type<T>) =>
  (res: unknown): TaskEither<ApplicationError, T> => {
    return pipe(
      te.fromEither(type.decode(res)),
      te.mapLeft(decodeError(prefix))
    );
  };

export const toFilterExpression = (name: string, value?: any): string[] =>
  typeof value === "undefined" ? [] : [`${name} = :${name}`];

export const toExpressionAttributeValueString = (
  name: string,
  value?: string
): object =>
  typeof value === "undefined"
    ? {}
    : {
        [`:${name}`]: {
          S: value,
        },
      };

export const toExpressionAttributeValueBool = (
  name: string,
  value?: boolean
): object =>
  typeof value === "undefined"
    ? {}
    : {
        [`:${name}`]: {
          BOOL: value,
        },
      };
