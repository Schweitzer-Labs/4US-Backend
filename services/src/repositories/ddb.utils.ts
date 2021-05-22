import * as t from "io-ts";
import { left, right, TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { isLeft } from "fp-ts/Either";

export const ddbString = t.type({
  S: t.string,
});

export const ddbBool = t.type({
  BOOL: t.boolean,
});

export const ddbNumber = t.type({
  N: t.string,
});

export const ddbStringList = t.type({
  SS: t.array(t.string),
});

export const validateDDBResponse =
  <T>(type: t.Type<T>) =>
  (res: any): TaskEither<ApplicationError, T> => {
    const eitherRes = type.decode(res);
    if (isLeft(eitherRes)) {
      return left(new ApplicationError("Invalid ddb response", {}));
    } else {
      return right(eitherRes.right);
    }
  };

export const extractDDBNumber = (obj: { N: string }): number => {
  return parseInt(obj?.N);
};

export const extractDDBString = (obj: { S: string }): string => {
  return obj?.S;
};

export const extractDDBBool = (obj: { BOOL: boolean }): boolean => {
  return obj?.BOOL;
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
