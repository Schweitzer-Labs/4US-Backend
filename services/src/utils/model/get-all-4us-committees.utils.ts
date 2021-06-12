import { DynamoDB } from "aws-sdk";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../application-error";
import {
  Committees,
  ICommittee,
} from "../../queries/get-committee-by-id.query";
import { Plan } from "../enums/plan.enum";
import { taskEither } from "fp-ts";
import { validateDDBResponse } from "../../repositories/ddb.utils";
import { pipe } from "fp-ts/function";

export const getAll4USCommittees =
  (committeesTableName: string) =>
  (dynamoDB: DynamoDB): TaskEither<ApplicationError, ICommittee[]> =>
    pipe(
      taskEither.tryCatch(
        () => queryDB(committeesTableName)(dynamoDB),
        (e) => new ApplicationError("Committee query failed", e)
      ),
      taskEither.chain(validateDDBResponse(Committees))
    );

const queryDB =
  (committeesTableName: string) =>
  async (dynamoDB: DynamoDB): Promise<any[]> => {
    const res = await dynamoDB
      .scan({
        TableName: committeesTableName,
        FilterExpression: "platformPlan = :platformPlan",
        ExpressionAttributeValues: {
          ":platformPlan": { S: Plan.FourUs },
        },
      })
      .promise();
    const committees = res.Items.map((item) =>
      DynamoDB.Converter.unmarshall(item)
    );
    return committees;
  };
