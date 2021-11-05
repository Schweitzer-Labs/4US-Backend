import { DynamoDB } from "aws-sdk";
import { TaskEither } from "fp-ts/TaskEither";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { ApplicationError } from "../../application-error";
import { decodeCommittees } from "./get-committee-by-id.query";
import { StatusCodes } from "http-status-codes";
import { ICommittee } from "../../../model/committee.type";
import { findOne } from "./find-one-committee.utils";

export const getCommitteesByActBlueIdAndDecode =
  (committeeTable: string) =>
  (dynamoDB: DynamoDB) =>
  (actBlueId: string): TaskEither<ApplicationError, ICommittee> =>
    pipe(
      taskEither.tryCatch(
        () => getCommitteesByActBlueId(committeeTable)(dynamoDB)(actBlueId),
        (err) =>
          new ApplicationError(
            "Get committee request failed",
            err,
            StatusCodes.OK
          )
      ),
      taskEither.chain(decodeCommittees),
      taskEither.chain(findOne)
    );

const getCommitteesByActBlueId =
  (committeesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (actBlueId: string): Promise<any> => {
    console.log("Get committees by ActBlue Id called", actBlueId);
    const res = await dynamoDB
      .scan({
        TableName: committeesTableName,
        FilterExpression: "actBlueId = :actBlueId",
        ExpressionAttributeValues: {
          ":actBlueId": { S: actBlueId },
        },
      })
      .promise();
    const committees = res.Items.map((item) =>
      DynamoDB.Converter.unmarshall(item)
    );

    console.log("Get committees by ActBlue Id res", JSON.stringify(committees));

    return committees;
  };
