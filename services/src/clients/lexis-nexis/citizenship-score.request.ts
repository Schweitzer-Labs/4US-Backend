import { ApplicationError } from "../../utils/application-error";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";

import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { isLeft } from "fp-ts/Either";
import { now } from "../../utils/time.utils";
import { IDonorInput } from "../../queries/search-donors.decoder";
import { DueDiligenceAttributesResponse } from "./lexis-nexis.decoder";
import { ICommittee } from "../../queries/get-committee-by-id.query";
import {
  BillableEventName,
  putBillableEvent,
} from "../../utils/model/put-billable-event.utils";
import { DynamoDB } from "aws-sdk";
import { genTxnId } from "../../utils/gen-txn-id.utils";
import axios from "axios";
import { decodeError } from "../../utils/decode-error.util";
import {
  dueDiligenceAttributesEndpoint,
  ILexisNexisConfig,
} from "./lexis-nexis.client";

const prefix = "Lexis-nexis citizenship score";

const formatRequest = (d: IDonorInput) => {
  const streetAddress2 = d.addressLine2
    ? { StreetAddress2: d.addressLine2 }
    : {};

  const phone = d.phoneNumber ? { Phone: d.phoneNumber } : {};

  return {
    DueDiligenceAttributesRequest: {
      ReportBy: {
        ProductRequestType: "CitizenshipOnly",
        Person: {
          ...phone,
          Address: {
            StreetAddress1: d.addressLine1,
            ...streetAddress2,
            City: d.city,
            State: d.state,
            Zip5: d.postalCode,
            PostalCode: d.postalCode,
          },
          Name: {
            First: d.firstName,
            Last: d.lastName,
          },
          Citizenship: {
            CitizenshipModelName: null,
          },
        },
      },
    },
  };
};

const runInstantId =
  (billableEventTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committee: ICommittee) =>
  (config: ILexisNexisConfig) =>
  async (donorInput: IDonorInput): Promise<any> => {
    console.log("Instance ID running");
    const payload = formatRequest(donorInput);
    const { data } = await axios.post(dueDiligenceAttributesEndpoint, payload, {
      auth: {
        username: config.username,
        password: config.password,
      },
    });

    await putBillableEvent(billableEventTableName)(dynamoDB)({
      id: genTxnId(),
      committeeId: committee.id,
      eventName: BillableEventName.LexisNexisCitizenshipLookUp,
      cost: 50,
      request: payload,
      response: data,
    });
    return data;
  };

export interface ICitizenshipScoreResult {
  citizenshipScore?: string;
  citizenshipScoreRawResponse: any;
  citizenshipScoreRequestTimestamp: number;
}

const resToCitizenshipScoreResult = (
  data: unknown
): TaskEither<ApplicationError, ICitizenshipScoreResult> => {
  const citizenshipScoreRequestTimestamp = now();
  const res = DueDiligenceAttributesResponse.decode(data);
  if (isLeft(res)) {
    new ApplicationError(
      "Invalid lexis-nexis response",
      decodeError(prefix)(res.left)
    );
    return taskEither.of({
      citizenshipScoreRawResponse: data,
      citizenshipScoreRequestTimestamp,
      citizenshipScore: "0",
    });
  } else {
    return taskEither.of({
      citizenshipScore:
        res.right.DueDiligenceAttributesResponseEx.response.Result
          .CitizenshipResults.CitizenshipScore,
      citizenshipScoreRawResponse: data,
      citizenshipScoreRequestTimestamp,
    });
  }
};

export const donorInputToCitizenshipScore =
  (billableEventsTable: string) =>
  (dynamoDB: DynamoDB) =>
  (config: ILexisNexisConfig) =>
  (committee: ICommittee) =>
  (
    donorInput: IDonorInput
  ): TaskEither<ApplicationError, ICitizenshipScoreResult> =>
    pipe(
      tryCatch(
        () =>
          runInstantId(billableEventsTable)(dynamoDB)(committee)(config)(
            donorInput
          ),
        (e) => new ApplicationError("ID verification look up failed", e)
      ),
      taskEither.chain(resToCitizenshipScoreResult)
    );
