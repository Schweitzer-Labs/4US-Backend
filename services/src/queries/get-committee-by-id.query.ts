import { DynamoDB } from "aws-sdk";
import * as t from "io-ts";
import { pipe } from "fp-ts/function";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { StatusCodes } from "http-status-codes";
import { taskEither as te, taskEither } from "fp-ts";
import { validateDDBResponse } from "../repositories/ddb.utils";
import { decodeError } from "../utils/decode-error.util";

import { isEmpty } from "../utils/model/get-res-is-empty.utils";

const logPrefix = "Get Committee";

const CommitteeRequired = t.type({
  id: t.string,
  committeeName: t.string,
  candidateFirstName: t.string,
  candidateLastName: t.string,
  stripeAccount: t.string,
  members: t.array(t.string),
  tzDatabaseName: t.string,
  platformPlan: t.string,
});

const CommitteeOptional = t.partial({
  candidateMiddleName: t.string,
  state: t.string,
  scope: t.string,
  officeType: t.string,
  party: t.string,
  race: t.string,
  district: t.string,
  county: t.string,
  bankName: t.string,
  ruleVersion: t.string,
  finicityCustomerId: t.string,
  finicityAccountId: t.string,
  chainId: t.string,
  emailAddresses: t.string,
  employmentStatus: t.string,
  efsFilerId: t.number,
  efsElectionId: t.number,
});

export const Committee = t.intersection([CommitteeRequired, CommitteeOptional]);

export const Committees = t.array(Committee);

export type ICommittee = t.TypeOf<typeof Committee>;

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

    console.log("Get committee res", committee);

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
