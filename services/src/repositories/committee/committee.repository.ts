import { ApplicationError } from "../../utils/application-error";
import { DynamoDB } from "aws-sdk";
import { TaskEither, right, tryCatch } from "fp-ts/TaskEither";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { Committee, DDBCommitteeRes } from "./committees.decoders";
import { validateDDBResponse } from "../ddb.utils";
import { StatusCodes } from "http-status-codes";

const logPrefix = "Get Committees";

const getCommitteesRes =
  (env = "dev") =>
  (dynamoDB: DynamoDB) =>
  async (): Promise<any> => {
    return await dynamoDB
      .executeStatement({
        //@ ToDo make table name configurable.
        Statement: `SELECT * FROM "committees-${env}"`,
      })
      .promise();
  };

const ddbResponseToCommittees = (
  ddbResponse: DDBCommitteeRes
): TaskEither<ApplicationError, Committee[]> => {
  const committees = ddbResponse.Items.map(
    ({
      committeeName: { S: committeeName },
      id: { S: id },
      stripeUserId: { S: stripeUserId },
      emailAddresses: { SS: emailAddresses },
    }) => ({
      committeeName,
      id,
      stripeUserId,
      emailAddresses,
    })
  );
  return right(committees);
};

export const getAllCommittees =
  (env: string) =>
  (dynamoDB: DynamoDB): TaskEither<ApplicationError, Committee[]> =>
    pipe(
      tryCatch<ApplicationError, any>(
        () => getCommitteesRes(env)(dynamoDB)(),
        (e) =>
          new ApplicationError(
            "Get committees request failed",
            e,
            StatusCodes.INTERNAL_SERVER_ERROR
          )
      ),
      taskEither.chain(validateDDBResponse(logPrefix)(DDBCommitteeRes)),
      taskEither.chain(ddbResponseToCommittees)
    );
