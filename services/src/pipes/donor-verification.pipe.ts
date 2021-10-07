import { DynamoDB } from "aws-sdk";
import { donorInputToInstantIdResult } from "../clients/lexis-nexis/consumer-id.request";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { pipe } from "fp-ts/function";
import { putDonor } from "../utils/model/put-donor.utils";
import { genTxnId } from "../utils/gen-txn-id.utils";
import { now } from "../utils/time.utils";
import { genFlacspee } from "../utils/model/gen-donor-match.utils";
import { taskEither } from "fp-ts";
import {
  donorInputToDonors,
  getDonorByLNId,
} from "../queries/search-donors.query";
import { IDonor, IDonorInput } from "../queries/search-donors.decoder";
import { ICommittee } from "../queries/get-committee-by-id.query";
import { Plan } from "../utils/enums/plan.enum";
import { ILexisNexisConfig } from "../clients/lexis-nexis/lexis-nexis.client";
import { donorToCitizenshipScore } from "../clients/lexis-nexis/citizenship-score.request";

const saveDonor =
  (donorsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (donor: IDonor): TaskEither<ApplicationError, IDonor> =>
    pipe(
      tryCatch(
        () => putDonor(donorsTableName)(dynamoDB)(donor),
        (e) => new ApplicationError("Put donor failed", e)
      )
    );

const donorInputToDonor = (donorInput: IDonorInput): IDonor => ({
  id: genTxnId(),
  createdTimestamp: now(),
  flacspeeMatch: genFlacspee(donorInput),
  ...donorInput,
});

const donorInputToVerifiedDonor =
  (billableEventsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (config: ILexisNexisConfig) =>
  (committee: ICommittee) =>
  (donorInput: IDonorInput): TaskEither<ApplicationError, IDonor> =>
    // Primitive feature configuration support
    committee.platformPlan === Plan.Policapital
      ? taskEither.of(donorInputToDonor(donorInput))
      : pipe(
          donorInputToInstantIdResult(billableEventsTableName)(dynamoDB)(
            config
          )(committee)(donorInput),
          taskEither.chain(
            donorToCitizenshipScore(billableEventsTableName)(dynamoDB)(config)(
              committee
            )
          )
        );

const verifyAndCreateDonorIfEmpty =
  (billableEventsTableName: string) =>
  (donorsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (config: ILexisNexisConfig) =>
  (committee: ICommittee) =>
  (donorInput: IDonorInput) =>
  (matchedDonors: IDonor[]): TaskEither<ApplicationError, IDonor> => {
    console.log("Donor verifier running");
    if (matchedDonors.length > 0) {
      console.log("Donor is matched");
      return taskEither.of(matchedDonors[0]);
    } else {
      return pipe(
        donorInputToVerifiedDonor(billableEventsTableName)(dynamoDB)(config)(
          committee
        )(donorInput),
        taskEither.chain(
          matchDonorWithVerifierOrSave(donorsTableName)(dynamoDB)
        )
      );
    }
  };

const matchDonorWithVerifierOrSave =
  (donorsTable: string) =>
  (ddb: DynamoDB) =>
  (donor: IDonor): TaskEither<ApplicationError, IDonor> =>
    pipe(
      donor.instantIdUniqueId
        ? getDonorByLNId(donorsTable)(ddb)(donor)
        : taskEither.of([]),
      taskEither.chain((donors) =>
        donors.length > 0
          ? taskEither.of(donors[0])
          : saveDonor(donorsTable)(ddb)(donor)
      )
    );

export const verifyDonor =
  (billableEventsTableName: string) =>
  (donorsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (config: ILexisNexisConfig) =>
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
