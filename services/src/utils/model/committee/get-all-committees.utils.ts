import { DynamoDB } from "aws-sdk";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../application-error";
import { Plan } from "../../enums/plan.enum";
import { taskEither } from "fp-ts";
import { validateDDBResponse } from "../../ddb.utils";
import { pipe } from "fp-ts/function";
import { Committees, ICommittee } from "../../../model/committee.type";

const logPrefix = "Get All Committees";

export const getAllCommitteesAndDecode =
  (committeesTableName: string) =>
  (dynamoDB: DynamoDB): TaskEither<ApplicationError, ICommittee[]> =>
    pipe(
      taskEither.tryCatch(
        () => getAllCommittees(committeesTableName)(dynamoDB),
        (e) => new ApplicationError("Committee query failed", e)
      ),
      taskEither.chain(validateDDBResponse(logPrefix)(Committees))
    );

const getAllCommittees =
  (committeesTableName: string) =>
  async (dynamoDB: DynamoDB): Promise<unknown[]> => {
    const res = await dynamoDB
      .scan({
        TableName: committeesTableName,
      })
      .promise();
    return res.Items.map((item) => DynamoDB.Converter.unmarshall(item));
  };
