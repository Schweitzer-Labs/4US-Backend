import { DynamoDB } from "aws-sdk";
import { donorInputToInstantIdResult } from "../clients/lexis-nexis/consumer-id.request";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { pipe } from "fp-ts/function";
import { putDonor } from "../utils/model/donor/put-donor.utils";
import { genTxnId } from "../utils/gen-txn-id.utils";
import { now } from "../utils/time.utils";
import { genFlacspee } from "../utils/model/donor/gen-donor-match.utils";
import { taskEither } from "fp-ts";
import {
  donorInputToDonors,
  getDonorByLNId,
} from "../utils/model/donor/search-donors.query";
import { IDonor, IDonorInput } from "../model/donor.type";
import { Plan } from "../utils/enums/plan.enum";
import { ILexisNexisConfig } from "../clients/lexis-nexis/lexis-nexis.client";
import { donorToCitizenshipScore } from "../clients/lexis-nexis/citizenship-score.request";
import { ICommittee } from "../model/committee.type";

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
  (idVerifyEnabled: boolean) =>
  (billableEventsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (config: ILexisNexisConfig) =>
  (committee: ICommittee) =>
  (donorInput: IDonorInput): TaskEither<ApplicationError, IDonor> =>
    // Primitive feature configuration support
    committee.platformPlan === Plan.Policapital || !idVerifyEnabled
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
  (idVerifyEnabled: boolean) =>
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
        donorInputToVerifiedDonor(idVerifyEnabled)(billableEventsTableName)(
          dynamoDB
        )(config)(committee)(donorInput),
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
  (idVerifyEnabled: boolean) =>
  (billableEventsTableName: string) =>
  (donorsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (config: ILexisNexisConfig) =>
  (committee: ICommittee) =>
  (donorInput: IDonorInput): TaskEither<ApplicationError, IDonor> =>
    pipe(
      donorInputToDonors(donorsTableName)(dynamoDB)(donorInput),
      taskEither.chain(
        verifyAndCreateDonorIfEmpty(idVerifyEnabled)(billableEventsTableName)(
          donorsTableName
        )(dynamoDB)(config)(committee)(donorInput)
      )
    );
