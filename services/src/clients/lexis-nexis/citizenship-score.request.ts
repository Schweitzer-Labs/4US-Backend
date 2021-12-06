import { ApplicationError } from "../../utils/application-error";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";

import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { isLeft } from "fp-ts/Either";
import { now } from "../../utils/time.utils";
import { IDonor, IDonorInput } from "../../model/donor.type";
import { DueDiligenceAttributesResponse } from "./lexis-nexis.decoder";
import {
  BillableEventName,
  putBillableEvent,
} from "../../utils/model/billable-events/put-billable-event.utils";
import { DynamoDB } from "aws-sdk";
import { genTxnId } from "../../utils/gen-txn-id.utils";
import axios from "axios";
import { decodeError } from "../../utils/decode-error.util";
import {
  dueDiligenceAttributesEndpoint,
  ILexisNexisConfig,
} from "./lexis-nexis.client";
import { ICommittee } from "../../model/committee.type";

const prefix = "Lexis-nexis citizenship score";

const formatRequest = (d: IDonor) => {
  const streetAddress2 = d.addressLine2
    ? { StreetAddress2: d.addressLine2 }
    : {};

  const phone = d.phoneNumber ? { Phone: d.phoneNumber } : {};

  return {
    DueDiligenceAttributesRequest: {
      Options: {
        DDAttributesVersionRequest: "DDAPERV3",
      },
      ReportBy: {
        ProductRequestType: "CitizenshipOnly",
        AttributeModules: {
          Entry: [
            {
              Name: null,
            },
          ],
        },
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
            CitizenshipModelName: "CIT1808_0_0",
          },
        },
      },
    },
  };
};

const run =
  (billableEventTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committee: ICommittee) =>
  (config: ILexisNexisConfig) =>
  async (donor: IDonor): Promise<any> => {
    console.log("Instance ID running");
    const payload = formatRequest(donor);
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
      // Scores range from 300 to 999.
      // 0 should indicate the context is dev.
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

const resToDonor =
  (donor: IDonor) =>
  (scoreRes: ICitizenshipScoreResult): IDonor => ({
    ...donor,
    ...scoreRes,
  });

export const donorToCitizenshipScore =
  (billableEventsTable: string) =>
  (dynamoDB: DynamoDB) =>
  (config: ILexisNexisConfig) =>
  (committee: ICommittee) =>
  (donor: IDonor): TaskEither<ApplicationError, IDonor> =>
    pipe(
      tryCatch(
        () => run(billableEventsTable)(dynamoDB)(committee)(config)(donor),
        (e) => new ApplicationError("Citizenship look up failed", e)
      ),
      taskEither.chain(resToCitizenshipScoreResult),
      taskEither.map(resToDonor(donor))
    );
