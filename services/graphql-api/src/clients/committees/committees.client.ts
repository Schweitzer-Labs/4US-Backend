import { ApplicationError } from "../../utils/application-error";
import * as D from "io-ts/Decoder";
import { DynamoDB } from "aws-sdk";
import { TaskEither, left, right, tryCatch } from "fp-ts/TaskEither";
import { isLeft } from "fp-ts/Either";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { Committee, DDBCommitteeRes } from "./committees.decoders";

const getCommitteesRes = (dynamoDB: DynamoDB) => async (): Promise<any> => {
  return await dynamoDB
    .executeStatement({
      //@ ToDo make table name configurable.
      Statement: 'SELECT * FROM "committees-dev"',
    })
    .promise();
};

const validateDDBResponse = (
  res: any
): TaskEither<ApplicationError, DDBCommitteeRes> => {
  const eitherCommitteesRes = DDBCommitteeRes.decode(res);
  if (isLeft(eitherCommitteesRes)) {
    return left(
      new ApplicationError("Invalid response", D.draw(eitherCommitteesRes.left))
    );
  } else {
    return right(eitherCommitteesRes.right);
  }
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

export const getAllCommittees = (
  dynamoDB: DynamoDB
): TaskEither<ApplicationError, Committee[]> =>
  pipe(
    tryCatch<ApplicationError, any>(
      () => getCommitteesRes(dynamoDB)(),
      (e) => new ApplicationError("Get committees request failed", e)
    ),
    taskEither.chain(validateDDBResponse),
    taskEither.chain(ddbResponseToCommittees)
  );
