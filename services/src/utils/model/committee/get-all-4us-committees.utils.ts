import { DynamoDB } from "aws-sdk";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../application-error";
import { Plan } from "../../enums/plan.enum";
import { taskEither } from "fp-ts";
import { validateDDBResponse } from "../../ddb.utils";
import { pipe } from "fp-ts/function";
import { Committees, ICommittee } from "../../../model/committee.type";

const logPrefix = "Get All 4US Committees";

export const getAll4USCommitteesAndDecode =
  (committeesTableName: string) =>
  (dynamoDB: DynamoDB): TaskEither<ApplicationError, ICommittee[]> =>
    pipe(
      taskEither.tryCatch(
        () => getAll4USCommittees(committeesTableName)(dynamoDB),
        (e) => new ApplicationError("Committee query failed", e)
      ),
      taskEither.chain(validateDDBResponse(logPrefix)(Committees))
    );

export const getAll4USCommittees =
  (committeesTableName: string) =>
  async (dynamoDB: DynamoDB): Promise<unknown[]> => {
    const res = await dynamoDB
      .scan({
        TableName: committeesTableName,
        FilterExpression: "platformPlan = :platformPlan",
        ExpressionAttributeValues: {
          ":platformPlan": { S: Plan.FourUs },
        },
      })
      .promise();
    return res.Items.map((item) => DynamoDB.Converter.unmarshall(item));
  };
