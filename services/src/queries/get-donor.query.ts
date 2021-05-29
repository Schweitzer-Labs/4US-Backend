import { DynamoDB } from "aws-sdk";
import * as t from "io-ts";
import { pipe } from "fp-ts/function";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { StatusCodes } from "http-status-codes";
import { taskEither } from "fp-ts";
import { validateDDBResponse } from "../repositories/ddb.utils";
import {
  genFlacspee,
  IFlacspeeInput,
} from "../utils/model/gen-donor-match.utils";

const DonorRequired = t.type({
  id: t.string,
  flacspeeMatch: t.string,
  emailAddress: t.string,
  createdTimestamp: t.number,
  firstName: t.string,
  lastName: t.string,
  addressLine1: t.string,
  city: t.string,
  state: t.string,
  postalCode: t.string,
  entityType: t.string,
});

const DonorOptional = t.partial({
  middleName: t.string,
  addressLine2: t.string,
  employer: t.string,
  occupation: t.string,
  companyName: t.string,
  phoneNumber: t.string,
  transactionType: t.string,
  attestsToBeingAnAdultCitizen: t.boolean,
  cardNumberLastFourDigits: t.string,
  entityName: t.string,
});

export const Donor = t.intersection([DonorRequired, DonorOptional]);

export type IDonor = t.TypeOf<typeof Donor>;

export const flacspeeMatchToDDBRes =
  (donorTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (flacspeeInput: IFlacspeeInput): Promise<any> => {
    const flacspeeMatch = genFlacspee(flacspeeInput);
    const res = await dynamoDB
      .getItem({
        TableName: donorTableName,
        Key: {
          flacspeeMatch: {
            S: flacspeeMatch,
          },
        },
      })
      .promise();
    return DynamoDB.Converter.unmarshall(res.Item);
  };

export const getDonorByFlacspee =
  (donorsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (flacspeeInput: IFlacspeeInput): TaskEither<ApplicationError, IDonor> => {
    return pipe(
      tryCatch<ApplicationError, any>(
        () => flacspeeMatchToDDBRes(donorsTableName)(dynamoDB)(flacspeeInput),
        (e) =>
          new ApplicationError(
            "Get donor request failed",
            e,
            StatusCodes.INTERNAL_SERVER_ERROR
          )
      ),
      taskEither.chain(validateDDBResponse(Donor))
    );
  };
