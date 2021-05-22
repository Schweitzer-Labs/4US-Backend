import { DynamoDB } from "aws-sdk";
import * as t from "io-ts";
import { Field, ID } from "type-graphql";
import { partial } from "io-ts";
import { pipe } from "fp-ts/function";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { StatusCodes } from "http-status-codes";
import { taskEither } from "fp-ts";
import { validateDDBResponse } from "../repositories/ddb.utils";
import { DDBCommitteeRes } from "../repositories/committee/committees.decoders";

const CommitteeRequired = t.type({
  id: t.string,
  committeeName: t.string,
  candidateFirstName: t.string,
  candidateLastName: t.string,
  stripeAccount: t.string,
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
  chainId: t.string,
  emailAddresses: t.string,
  tzDatabaseName: t.string,
  plan: t.string,
});

export const Committee = t.intersection([CommitteeRequired, CommitteeOptional]);

export type ICommittee = t.TypeOf<typeof Committee>;

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
    const unmarshalled = DynamoDB.Converter.unmarshall(res.Item);
    return unmarshalled;
  };

export const getCommitteeById =
  (committeesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string): TaskEither<ApplicationError, ICommittee> => {
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
      taskEither.chain(validateDDBResponse(Committee))
    );
  };
