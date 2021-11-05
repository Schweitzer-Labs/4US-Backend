import { ApplicationError } from "../../utils/application-error";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";

import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { isLeft } from "fp-ts/Either";
import { now } from "../../utils/time.utils";
import { BusinessIDResponse } from "./lexis-nexis.decoder";
import {
  BillableEventName,
  putBillableEvent,
} from "../../utils/model/billable-events/put-billable-event.utils";
import { DynamoDB } from "aws-sdk";
import { genTxnId } from "../../utils/gen-txn-id.utils";
import axios from "axios";
import { decodeError } from "../../utils/decode-error.util";
import {
  businessIdEndpoint,
  ILexisNexisConfig,
  logPrefix,
} from "./lexis-nexis.client";
import { ITransaction } from "../../model/transaction.type";
import { ICommittee } from "../../model/committee.type";

const formatRequest = (d: ITransaction) => {
  const streetAddress2 = d.addressLine2
    ? { StreetAddress2: d.addressLine2 }
    : {};

  return {
    BusinessInstantID20Request: {
      Options: {
        BIID20ProductType: 0,
      },
      SearchBy: {
        Company: {
          CompanyName: d.entityName,
          Address: {
            StreetAddress1: d,
            ...streetAddress2,
            City: d.city,
            State: d.state,
            Zip5: d.postalCode,
          },
        },
      },
    },
  };
};

const runLookUp =
  (billableEventTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committee: ICommittee) =>
  (config: ILexisNexisConfig) =>
  async (input: ITransaction): Promise<unknown> => {
    console.log("Instance ID running");
    const payload = formatRequest(input);
    const { data } = await axios.post(businessIdEndpoint, payload, {
      auth: {
        username: config.username,
        password: config.password,
      },
    });

    await putBillableEvent(billableEventTableName)(dynamoDB)({
      id: genTxnId(),
      committeeId: committee.id,
      eventName: BillableEventName.LexisNexisBusinessIdLookUp,
      cost: 200,
      request: payload,
      response: data,
    });
    return data;
  };

export interface IBusinessIDResult {
  businessIdVerificationScore?: string;
  businessIdRawResponse: any;
  businessIdRequestTimestamp: number;
}

const resToTxn =
  (txn: ITransaction) =>
  (data: unknown): TaskEither<ApplicationError, ITransaction> => {
    const businessIdRequestTimestamp = now();
    const res = BusinessIDResponse.decode(data);
    if (isLeft(res)) {
      new ApplicationError(
        "Invalid lexis-nexis response",
        decodeError(logPrefix)(res.left)
      );
      return taskEither.of({
        ...txn,
        businessIdRawResponse: data,
        businessIdRequestTimestamp,
        businessIdComprehensiveVerificationScore: 0,
      });
    } else {
      return taskEither.of({
        ...txn,
        businessIdVerificationScore:
          res.right.BIID20ResponseEx.response.Result.CompanyResults
            .BusinessVerification.Index,
        businessIdRawResponse: data,
        businessIdRequestTimestamp,
      });
    }
  };

export const runBIDonTxn =
  (billableEventsTable: string) =>
  (dynamoDB: DynamoDB) =>
  (config: ILexisNexisConfig) =>
  (committee: ICommittee) =>
  (txn: ITransaction): TaskEither<ApplicationError, ITransaction> =>
    pipe(
      tryCatch(
        () => runLookUp(billableEventsTable)(dynamoDB)(committee)(config)(txn),
        (e) =>
          new ApplicationError("Business ID verification look up failed", e)
      ),
      taskEither.chain(resToTxn(txn))
    );
