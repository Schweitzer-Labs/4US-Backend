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
import { ICommittee } from "../queries/get-committee-by-id.query";

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
  (billableEventsTableName: string) =>
  (donorsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (config: IInstantIdConfig) =>
  (committee: ICommittee) =>
  (donorInput: IDonorInput) =>
  (matchedDonors: IDonor[]): TaskEither<ApplicationError, IDonor> => {
    if (matchedDonors.length > 0) {
      return taskEither.of(matchedDonors[0]);
    } else {
      return pipe(
        donorInputToInstantIdResult(billableEventsTableName)(dynamoDB)(config)(
          committee
        )(donorInput),
        taskEither.chain(
          instantIdResultToNewDonor(donorsTableName)(dynamoDB)(donorInput)
        )
      );
    }
  };

export const verifyDonor =
  (billableEventsTableName: string) =>
  (donorsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (config: IInstantIdConfig) =>
  (committee: ICommittee) =>
  (donorInput: IDonorInput): TaskEither<ApplicationError, IDonor> =>
    pipe(
      donorInputToDonors(donorsTableName)(dynamoDB)(donorInput),
      taskEither.chain(
        verifyAndCreateDonorIfEmpty(billableEventsTableName)(donorsTableName)(
          dynamoDB
        )(config)(committee)(donorInput)
      )
    );
