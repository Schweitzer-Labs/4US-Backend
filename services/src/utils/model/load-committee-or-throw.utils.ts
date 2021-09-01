import { DynamoDB } from "aws-sdk";
import {
  getCommitteeById,
  ICommittee,
} from "../../queries/get-committee-by-id.query";
import { isLeft } from "fp-ts/Either";
import { UnauthorizedError } from "type-graphql";
import { ApplicationError } from "../application-error";
import { StatusCodes } from "http-status-codes";

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
