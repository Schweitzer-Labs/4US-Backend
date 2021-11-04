import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../application-error";
import { taskEither as te } from "fp-ts";
import { IAggs } from "../../types/aggs.type";

export const putAggs =
  (aggsTable: string) => (dynamoDB: DynamoDB) => async (aggs: IAggs) => {
    console.log("Agg writing to ddb");
    console.log("Agg is being put", aggs);
    const marshalledAggs = DynamoDB.Converter.marshall(aggs);
    await dynamoDB
      .putItem({
        TableName: aggsTable,
        Item: marshalledAggs,
      })
      .promise();
    return aggs;
  };

export const putAggAndDecode =
  (aggTable: string) =>
  (dynamoDB: DynamoDB) =>
  (aggs: IAggs): TaskEither<ApplicationError, IAggs> =>
    pipe(
      te.tryCatch(
        () => putAggs(aggTable)(dynamoDB)(aggs),
        (e) => new ApplicationError("Put transaction failed", e)
      )
    );
