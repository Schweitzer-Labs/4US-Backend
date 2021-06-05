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
    console.log("from load committee", committeeId, committeeTableName);
    const eitherCommittees = await getCommitteeById(committeeTableName)(
      dynamoDB
    )(committeeId)();
    console.log("eitherCommittees set", JSON.stringify(eitherCommittees));
    if (isLeft(eitherCommittees)) {
      console.log("error gets thrown, committees is left");
      throw new ApplicationError(
        "Committee look up failed",
        {},
        StatusCodes.NOT_FOUND
      );
    }
    const committee = eitherCommittees.right;
    if (!committee.members.includes(currentUser)) {
      console.log("api use is not authorized");
      throw new UnauthorizedError();
    }

    console.log("committee has loaded", committee);

    return committee;
  };
