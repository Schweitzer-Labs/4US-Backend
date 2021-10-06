import { DynamoDB } from "aws-sdk";
import {
  getCommitteeById,
  ICommittee,
} from "../../queries/get-committee-by-id.query";
import { isLeft } from "fp-ts/Either";
import { UnauthorizedError } from "type-graphql";
import { ApplicationError } from "../application-error";
import { StatusCodes } from "http-status-codes";
import { TaskEither } from "fp-ts/TaskEither";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";

export const loadCommitteeOrError =
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string) =>
  (currentUser: string): TaskEither<ApplicationError, ICommittee> =>
    pipe(
      getCommitteeById(committeeTableName)(dynamoDB)(committeeId),
      taskEither.chain(validateUser(currentUser))
    );

const validateUser =
  (currentUser: string) =>
  (committee: ICommittee): TaskEither<ApplicationError, ICommittee> => {
    if (!committee.members.includes(currentUser)) {
      taskEither.left(new ApplicationError("API use is not authorized", {}));
    }
    return taskEither.right(committee);
  };

export const loadCommitteeOrThrow =
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string) =>
  async (currentUser: string): Promise<ICommittee> => {
    const eitherCommittees = await getCommitteeById(committeeTableName)(
      dynamoDB
    )(committeeId)();
    if (isLeft(eitherCommittees)) {
      throw new ApplicationError(
        "Committee look up failed",
        eitherCommittees.left,
        StatusCodes.NOT_FOUND
      );
    }
    const committee = eitherCommittees.right;
    if (!committee.members.includes(currentUser)) {
      console.log("API use is not authorized");
      throw new UnauthorizedError();
    }

    return committee;
  };
