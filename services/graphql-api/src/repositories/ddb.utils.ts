import * as t from "io-ts";
import { left, right, TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { isLeft } from "fp-ts/Either";

export const ddbString = t.type({
  S: t.string,
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
