import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { StatusCodes } from "http-status-codes";
import { taskEither } from "fp-ts";
import { ApplicationError } from "../application-error";
import { Aggs, IAggs } from "../../types/aggs.type";
import { isEmpty } from "./get-res-is-empty.utils";
import { validateDDBResponse } from "../../repositories/ddb.utils";
import * as t from "io-ts";

const logPrefix = "Get Aggs";

export const committeeIdToDDBRes =
  (aggTable: string) =>
  (dynamoDB: DynamoDB) =>
  async (committeeId: string): Promise<any> => {
    const res = await dynamoDB
      .getItem({
        TableName: aggTable,
        Key: {
          committeeId: {
            S: committeeId,
          },
        },
      })
      .promise();

    const aggs = DynamoDB.Converter.unmarshall(res.Item);

    return aggs;
  };

export const getAggsByCommitteeId =
  (aggsTable: string) =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string): TaskEither<ApplicationError, IAggs> => {
    console.log("Get committee query called");
    return pipe(
      tryCatch<ApplicationError, any>(
        () => committeeIdToDDBRes(aggsTable)(dynamoDB)(committeeId),
        (e) =>
          new ApplicationError(
            "Get committee request failed",
            e,
            StatusCodes.INTERNAL_SERVER_ERROR
          )
      ),
      taskEither.map(zeroEmpties(committeeId)),
      taskEither.chain(validateDDBResponse(logPrefix)(Aggs))
    );
  };

export const zeroEmpties =
  (committeeId: string) =>
  (val: IAggs): IAggs => {
    return Object.keys(val)?.length > 0
      ? val
      : {
          balance: 0,
          totalRaised: 0,
          totalSpent: 0,
          totalDonors: 0,
          totalTransactions: 0,
          totalContributionsInProcessing: 0,
          totalDisbursementsInProcessing: 0,
          needsReviewCount: 0,
          committeeId,
        };
  };
