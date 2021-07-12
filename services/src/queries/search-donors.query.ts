import { DynamoDB } from "aws-sdk";
import * as t from "io-ts";
import { pipe } from "fp-ts/function";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { StatusCodes } from "http-status-codes";
import { taskEither } from "fp-ts";
import { validateDDBResponse } from "../repositories/ddb.utils";
import { genFlacspee } from "../utils/model/gen-donor-match.utils";
import { Donors, IDonor, IDonorInput } from "./search-donors.decoder";

const logPrefix = "Get Donors";

const queryDDB =
  (donorsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (donorInput: IDonorInput): Promise<unknown> => {
    const res = await dynamoDB
      .query({
        TableName: donorsTableName,
        KeyConditionExpression: "flacspeeMatch = :flacspeeMatch",
        ScanIndexForward: false,
        ExpressionAttributeValues: {
          ":flacspeeMatch": { S: genFlacspee(donorInput) },
        },
      })
      .promise();
    return res.Items.map((item) => DynamoDB.Converter.unmarshall(item));
  };

export const donorInputToDonors =
  (donorsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (donorInput: IDonorInput): TaskEither<ApplicationError, IDonor[]> => {
    return pipe(
      tryCatch<ApplicationError, any>(
        () => queryDDB(donorsTableName)(dynamoDB)(donorInput),
        (e) =>
          new ApplicationError(
            "Get donor request failed",
            e,
            StatusCodes.INTERNAL_SERVER_ERROR
          )
      ),
      taskEither.chain(validateDDBResponse(logPrefix)(Donors))
    );
  };
