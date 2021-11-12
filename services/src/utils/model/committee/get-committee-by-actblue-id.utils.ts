import { DynamoDB } from "aws-sdk";
import { TaskEither } from "fp-ts/TaskEither";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { ApplicationError } from "../../application-error";
import { decodeCommittees } from "./get-committee-by-id.query";
import { StatusCodes } from "http-status-codes";
import { ICommittee } from "../../../model/committee.type";
import { findOne } from "./find-one-committee.utils";

export const getCommitteeByActBlueAccountIdAndDecode =
  (committeeTable: string) =>
  (dynamoDB: DynamoDB) =>
  (actBlueAccountId: string): TaskEither<ApplicationError, ICommittee> =>
    pipe(
      taskEither.tryCatch(
        () =>
          getCommitteesByActBlueAccountId(committeeTable)(dynamoDB)(
            actBlueAccountId
          ),
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

const getCommitteesByActBlueAccountId =
  (committeesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (actBlueAccountId: string): Promise<any> => {
    console.log("Get committees by ActBlue Id called", actBlueAccountId);
    const res = await dynamoDB
      .scan({
        TableName: committeesTableName,
        FilterExpression: "actBlueAccountId = :actBlueAccountId",
        ExpressionAttributeValues: {
          ":actBlueAccountId": { S: actBlueAccountId },
        },
      })
      .promise();
    const committees = res.Items.map((item) =>
      DynamoDB.Converter.unmarshall(item)
    );

    console.log("Get committees by ActBlue Id res", JSON.stringify(committees));

    return committees;
  };
