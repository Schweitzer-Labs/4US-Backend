import { DynamoDB } from "aws-sdk";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../application-error";
import { pipe } from "fp-ts/function";
import { taskEither as te } from "fp-ts";
import { ICommittee } from "../../types/committee.type";

export const putCommittee =
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (committee: ICommittee) => {
    const marshalledCommittee = DynamoDB.Converter.marshall(committee);
    await dynamoDB
      .putItem({
        TableName: committeeTableName,
        Item: marshalledCommittee,
      })
      .promise();
    return committee;
  };

export const putCommitteeAndDecode =
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committee: ICommittee): TaskEither<ApplicationError, ICommittee> =>
    pipe(
      te.tryCatch(
        () => putCommittee(committeeTableName)(dynamoDB)(committee),
        (e) => new ApplicationError("Put committee failed", e)
      )
    );
