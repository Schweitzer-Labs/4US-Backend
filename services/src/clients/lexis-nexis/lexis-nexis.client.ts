import { ApplicationError } from "../../utils/application-error";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";

import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { isLeft } from "fp-ts/Either";
import { PathReporter } from "io-ts/PathReporter";
import { now } from "../../utils/time.utils";
import { IDonorInput } from "../../queries/search-donors.decoder";
import { InstantIdResponse } from "./lexis-nexis.decoder";
import { ICommittee } from "../../queries/get-committee-by-id.query";
import {
  BillableEventName,
  putBillableEvent,
} from "../../utils/model/put-billable-event.utils";
import { DynamoDB } from "aws-sdk";
import { genTxnId } from "../../utils/gen-txn-id.utils";
import axios from "axios";

const instantIdEndpoint =
  "https://wsonline.seisint.com/WsIdentity/InstantID?&ver_=2.80&_product_code=false&json_test_";

const formatInstantIdRequest = (d: IDonorInput) => {
  const streetAddress2 = d.addressLine2 ? { StreetAddress2: "2FL" } : {};
  const email = d.emailAddress ? { Email: d.emailAddress } : {};
  const homePhone = d.phoneNumber ? { HomePhone: d.phoneNumber } : {};

  return {
    InstantIDRequest: {
      Options: {
        WatchLists: {
          WatchList: [null],
        },
        DOBMatch: {
          MatchType: "FuzzyCCYYMMDD",
        },
        IncludeModels: {
          FraudPointModel: {
            IncludeRiskIndices: true,
          },
        },
        RequireExactMatch: {
          LastName: true,
          FirstName: true,
          Address: true,
        },
        NameInputOrder: "Unknown",
        IncludeEmailVerification: true,
      },
      SearchBy: {
        Name: {
          First: d.firstName,
          Last: d.lastName,
        },
        Address: {
          StreetAddress1: d.addressLine1,
          ...streetAddress2,
          City: d.city,
          State: d.state,
          Zip5: d.postalCode,
        },
        ...email,
        ...homePhone,
      },
    },
  };
};

export interface IInstantIdConfig {
  username: string;
  password: string;
}

const runInstantId =
  (billableEventTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committee: ICommittee) =>
  (config: IInstantIdConfig) =>
  async (donorInput: IDonorInput): Promise<any> => {
    const payload = formatInstantIdRequest(donorInput);
    const { data } = await axios.post(instantIdEndpoint, payload, {
      auth: {
        username: config.username,
        password: config.password,
      },
    });

    await putBillableEvent(billableEventTableName)(dynamoDB)({
      id: genTxnId(),
      committeeId: committee.id,
      eventName: BillableEventName.LexisNexisInstantIdLookUp,
      cost: 75,
      request: payload,
      response: data,
    });
    return data;
  };

export interface IInstantIdResult {
  instantIdComprehensiveVerificationScore?: number;
  instantIdUniqueId?: string;
  instantIdRawResponse: any;
  instantIdRequestTimestamp: number;
}

const resToInstantIdResult = (
  data: unknown
): TaskEither<ApplicationError, IInstantIdResult> => {
  const instantIdRequestTimestamp = now();
  const res = InstantIdResponse.decode(data);
  if (isLeft(res)) {
    new ApplicationError(
      "Invalid lexis-nexis response",
      PathReporter.report(res)
    );
    return taskEither.of({
      instantIdRawResponse: data,
      instantIdRequestTimestamp,
    });
  } else {
    return taskEither.of({
      instantIdComprehensiveVerificationScore:
        res.right.InstantIDResponseEx.response.Result.ComprehensiveVerification
          .ComprehensiveVerificationIndex,
      instantIdUniqueId: res.right.InstantIDResponseEx.response.Result.UniqueId,
      instantIdRawResponse: data,
      instantIdRequestTimestamp,
    });
  }
};

export const donorInputToInstantIdResult =
  (billableEventsTable: string) =>
  (dynamoDB: DynamoDB) =>
  (config: IInstantIdConfig) =>
  (committee: ICommittee) =>
  (donorInput: IDonorInput): TaskEither<ApplicationError, IInstantIdResult> =>
    pipe(
      tryCatch(
        () =>
          runInstantId(billableEventsTable)(dynamoDB)(committee)(config)(
            donorInput
          ),
        (e) => new ApplicationError("ID verification look up failed", e)
      ),
      taskEither.chain(resToInstantIdResult)
    );
