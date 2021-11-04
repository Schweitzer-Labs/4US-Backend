import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../application-error";
import { StatusCodes } from "http-status-codes";
import { taskEither as te, taskEither } from "fp-ts";
import { validateDDBResponse } from "../ddb.utils";
import { decodeError } from "../decode-error.util";

import { isEmpty } from "./get-res-is-empty.utils";
import { Committee, Committees, ICommittee } from "../../types/committee.type";

const logPrefix = "Get Committee";

export const decodeCommittees = (
  res: unknown
): TaskEither<ApplicationError, ICommittee[]> => {
  return pipe(
    te.fromEither(Committees.decode(res)),
    te.mapLeft(decodeError("Committees"))
  );
};

export const committeeIdToDDBRes =
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (committeeId: string): Promise<any> => {
    const res = await dynamoDB
      .getItem({
        TableName: committeeTableName,
        Key: {
          id: {
            S: committeeId,
          },
        },
      })
      .promise();

    const committee = DynamoDB.Converter.unmarshall(res.Item);

    return committee;
  };

export const getCommitteeById =
  (committeesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string): TaskEither<ApplicationError, ICommittee> => {
    console.log("Get committee query called");

    return pipe(
      tryCatch<ApplicationError, any>(
        () => committeeIdToDDBRes(committeesTableName)(dynamoDB)(committeeId),
        (e) =>
          new ApplicationError(
            "Get committee request failed",
            e,
            StatusCodes.INTERNAL_SERVER_ERROR
          )
      ),
      taskEither.chain(isEmpty(logPrefix)),
      taskEither.chain(validateDDBResponse(logPrefix)(Committee))
    );
  };
