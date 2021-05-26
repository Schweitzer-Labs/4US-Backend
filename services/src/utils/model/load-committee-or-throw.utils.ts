import { DynamoDB } from "aws-sdk";
import {
  getCommitteeById,
  ICommittee,
} from "../../queries/get-committee-by-id.query";
import { isLeft } from "fp-ts/Either";
import { UnauthorizedError } from "type-graphql";

export const loadCommitteeOrThrow =
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string) =>
  async (currentUser: string): Promise<ICommittee> => {
    const eitherCommittees = await getCommitteeById(committeeTableName)(
      dynamoDB
    )(committeeId)();
    if (isLeft(eitherCommittees)) {
      throw eitherCommittees.left;
    }
    const committee = eitherCommittees.right;
    if (!committee.members.includes(currentUser)) {
      throw new UnauthorizedError();
    }

    return committee;
  };
