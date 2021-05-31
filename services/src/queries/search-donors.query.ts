import { DynamoDB } from "aws-sdk";
import * as t from "io-ts";
import { pipe } from "fp-ts/function";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { StatusCodes } from "http-status-codes";
import { taskEither } from "fp-ts";
import { validateDDBResponse } from "../repositories/ddb.utils";
import { genFlacspee } from "../utils/model/gen-donor-match.utils";
import { EntityType } from "../utils/enums/entity-type.enum";

export interface IDonorInput {
  firstName: string;
  lastName: string;
  addressLine1: string;
  city: string;
  state: string;
  postalCode: string;
  entityType: EntityType;
  id?: string;
  emailAddress?: string;
  middleName?: string;
  addressLine2?: string;
  employer?: string;
  occupation?: string;
  companyName?: string;
  phoneNumber?: string;
  attestsToBeingAnAdultCitizen?: boolean;
  cardNumberLastFourDigits?: string;
  entityName?: string;
}

const DonorRequired = t.type({
  id: t.string,
  flacspeeMatch: t.string,
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
  emailAddress: t.string,
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
  instantIdComprehensiveVerificationScore: t.number,
  instantIdUniqueId: t.string,
  instantIdRawResponse: t.unknown,
  instantIdRequestTimestamp: t.number,
});

export const Donor = t.intersection([DonorRequired, DonorOptional]);
export const Donors = t.array(Donor);

export type IDonor = t.TypeOf<typeof Donor>;

const queryDDB =
  (donorsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (donorInput: IDonorInput): Promise<unknown> => {
    console.log(donorsTableName);
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
    console.log(res);
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
      taskEither.chain(validateDDBResponse(Donors))
    );
  };
