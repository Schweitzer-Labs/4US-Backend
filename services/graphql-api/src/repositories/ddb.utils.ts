import * as t from "io-ts";
import { left, right, TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { isLeft } from "fp-ts/Either";

export const ddbString = t.type({
  S: t.string,
});

export const ddbBool = t.type({
  BOOL: t.string,
});

export const ddbNumber = t.type({
  N: t.number,
});

export const ddbStringList = t.type({
  SS: t.array(t.string),
});

export const validateDDBResponse =
  <T>(type: t.Type<T>) =>
  (res: any): TaskEither<ApplicationError, T> => {
    const eitherCommitteesRes = type.decode(res);
    if (isLeft(eitherCommitteesRes)) {
      return left(new ApplicationError("Invalid response", {}));
    } else {
      return right(eitherCommitteesRes.right);
    }
  };

export const extractDDBNumber = (obj: { N: number }): number => {
  return obj?.N;
};

export const extractDDBString = (obj: { S: string }): string => {
  return obj?.S;
};

export const extractDDBBool = (obj: { BOOL: string }): boolean => {
  return obj?.BOOL === "true" || false;
};
