import "reflect-metadata";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";
import { successResponse } from "./utils/success-response";
import { appErrorToResp, errorResponse } from "./utils/error-response.utils";
import { StatusCodes } from "http-status-codes";
import { headers } from "./utils/headers";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "./utils/application-error";
import { eventToObject } from "./utils/event-to-object.util";
import { validateObject } from "./utils/validate-schema.utils";
import { externalDataSchema } from "./utils/external-data-schema.utils";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const dynamoDB = new DynamoDB();

const billableEventsTableName: any = process.env.BILLABLE_EVENTS_DDB_TABLE_NAME;
const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const donorsTableName: any = process.env.DONORS_DDB_TABLE_NAME;
const rulesTableName: any = process.env.RULES_DDB_TABLE_NAME;
const runenv: any = process.env.RUNENV;
const corsOrigin = process.env.CORS_ORIGIN;

const ps = new AWS.SSM();

const appKey = "0e6fa92b-0ab1-4d38-a521-67579010abaa";

const getAppKey = (event: any): any =>
  event?.headers ? event?.headers["4us-app-key"] : false;

const validateAppKey = (event: any): TaskEither<ApplicationError, object> =>
  getAppKey(event) === appKey
    ? taskEither.right(event)
    : taskEither.left(
        new ApplicationError(
          `Invalid auth. Please ensure headers contain property "4US-App-Key"`,
          {},
          StatusCodes.UNAUTHORIZED
        )
      );

const bluelinkEventToModel = (
  event: any
): TaskEither<ApplicationError, object> =>
  pipe(
    validateAppKey(event),
    taskEither.chain(eventToObject),
    taskEither.chain(validateObject(externalDataSchema))
  );

export default async (event: any) => {
  console.log("bluelink webhook called");
  console.log(JSON.stringify(event));

  return await pipe(
    bluelinkEventToModel(event),
    taskEither.fold(
      (error) =>
        task.of({
          ...appErrorToResp(error),
          headers: headers(corsOrigin),
        }),
      () =>
        task.of({
          ...successResponse,
          headers: headers(corsOrigin),
        })
    )
  )();
};
