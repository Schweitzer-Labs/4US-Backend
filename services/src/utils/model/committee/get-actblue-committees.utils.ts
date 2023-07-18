import { DynamoDB } from "aws-sdk";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../application-error";
import { taskEither } from "fp-ts";
import { validateDDBResponse } from "../../ddb.utils";
import { pipe } from "fp-ts/function";
import { Committees, ICommittee } from "../../../model/committee.type";

const logPrefix = "Get All 4US Committees";

export const getActBlueCommitteesAndDecode =
  (committeesTableName: string) =>
  (dynamoDB: DynamoDB): TaskEither<ApplicationError, ICommittee[]> =>
    pipe(
      taskEither.tryCatch(
        () => getActBlueCommittees(committeesTableName)(dynamoDB),
        (e) => new ApplicationError("Committee query failed", e)
      ),
      taskEither.chain(validateDDBResponse(logPrefix)(Committees))
    );

export const getActBlueCommittees =
  (committeesTableName: string) =>
  async (dynamoDB: DynamoDB): Promise<unknown[]> => {
    const res = await dynamoDB
      .scan({
        TableName: committeesTableName,
        IndexName: "CommitteesByActBlueAccountId",
      })
      .promise();
    return res.Items.map((item) => DynamoDB.Converter.unmarshall(item));
  };
