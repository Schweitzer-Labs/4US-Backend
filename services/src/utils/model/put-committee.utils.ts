import { DynamoDB } from "aws-sdk";
import { Committee, ICommittee } from "../../queries/get-committee-by-id.query";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../application-error";
import { pipe } from "fp-ts/function";
import { taskEither as te } from "fp-ts";

export const putCommittee =
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (committee: ICommittee) => {
    console.log("put committee might be failing", committee);
    const marshalledCommittee = DynamoDB.Converter.marshall(committee);
    console.log("marshalled committee", JSON.stringify(marshalledCommittee));
    await dynamoDB
      .putItem({
        TableName: committeeTableName,
        Item: marshalledCommittee,
      })
      .promise();
    console.log("put committee ddb call passed");
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
