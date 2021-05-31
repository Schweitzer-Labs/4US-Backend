import { DynamoDB } from "aws-sdk";
import {
  donorInputToInstantIdResult,
  IInstantIdConfig,
  IInstantIdResult,
} from "../clients/lexis-nexis/lexis-nexis.client";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { pipe } from "fp-ts/function";
import { putDonor } from "../utils/model/put-donor.utils";
import { genTxnId } from "../utils/gen-txn-id.utils";
import { now } from "../utils/time.utils";
import { genFlacspee } from "../utils/model/gen-donor-match.utils";
import { taskEither } from "fp-ts";
import { donorInputToDonors } from "../queries/search-donors.query";
import { IDonor, IDonorInput } from "../queries/search-donors.decoder";

const instantIdResultToNewDonor =
  (donorsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (donorInput: IDonorInput) =>
  (instantIdResult: IInstantIdResult): TaskEither<ApplicationError, IDonor> =>
    pipe(
      tryCatch(
        () =>
          putDonor(donorsTableName)(dynamoDB)({
            id: genTxnId(),
            createdTimestamp: now(),
            flacspeeMatch: genFlacspee(donorInput),
            ...donorInput,
            ...instantIdResult,
          }),
        (e) => new ApplicationError("Put donor failed", e)
      )
    );

const verifyAndCreateDonorIfEmpty =
  (donorsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (config: IInstantIdConfig) =>
  (donorInput: IDonorInput) =>
  (matchedDonors: IDonor[]): TaskEither<ApplicationError, IDonor> => {
    if (matchedDonors.length > 0) {
      return taskEither.of(matchedDonors[0]);
    } else {
      return pipe(
        donorInputToInstantIdResult(config)(donorInput),
        taskEither.chain(
          instantIdResultToNewDonor(donorsTableName)(dynamoDB)(donorInput)
        )
      );
    }
  };

export const verifyDonor =
  (donorsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (config: IInstantIdConfig) =>
  (donorInput: IDonorInput): TaskEither<ApplicationError, IDonor> =>
    pipe(
      donorInputToDonors(donorsTableName)(dynamoDB)(donorInput),
      taskEither.chain(
        verifyAndCreateDonorIfEmpty(donorsTableName)(dynamoDB)(config)(
          donorInput
        )
      )
    );
