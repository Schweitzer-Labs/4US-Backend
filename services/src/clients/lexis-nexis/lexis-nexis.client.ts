import { Env } from "../../utils/enums/env.enum";
import { ApplicationError } from "../../utils/application-error";
import { left, TaskEither, tryCatch } from "fp-ts/TaskEither";
import * as t from "io-ts";

import { mockRes } from "./lexis-nexis.mock";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { isLeft } from "fp-ts/Either";
import { PathReporter } from "io-ts/PathReporter";
import { now } from "../../utils/time.utils";
import { IDonorInput } from "../../queries/search-donors.decoder";

const instantIdEndpoint =
  "'https://wsonline.seisint.com/WsIdentity/InstantID?&ver_=2.80&_product_code=false&json_test_'";

const InstantIdResponse = t.type({
  InstantIDResponseEx: t.type({
    response: t.type({
      Result: t.type({
        UniqueId: t.string,
        ComprehensiveVerification: t.type({
          ComprehensiveVerificationIndex: t.number,
        }),
      }),
    }),
  }),
});

type IInstantIdResponse = t.TypeOf<typeof InstantIdResponse>;

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

const post = async (url: string, payload: any, options: object) => {
  return mockRes;
};

export interface IInstantIdConfig {
  username: string;
  password: string;
  env: Env;
}

const runInstantId =
  (config: IInstantIdConfig) =>
  async (donorInput: IDonorInput): Promise<any> => {
    const payload = formatInstantIdRequest(donorInput);

    if ([Env.Dev, Env.QA].includes(config.env)) {
      return await post(instantIdEndpoint, payload, {
        auth: {
          username: config.username,
          password: config.password,
        },
      });
    } else {
      throw new ApplicationError("Failure due to missing config", {});
    }
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
  (config: IInstantIdConfig) =>
  (donorInput: IDonorInput): TaskEither<ApplicationError, IInstantIdResult> =>
    pipe(
      tryCatch(
        () => runInstantId(config)(donorInput),
        (e) => new ApplicationError("ID verification look up failed", e)
      ),
      taskEither.chain(resToInstantIdResult)
    );
