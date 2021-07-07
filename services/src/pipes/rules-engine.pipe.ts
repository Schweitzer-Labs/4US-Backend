import { pipe } from "fp-ts/function";
import { DynamoDB } from "aws-sdk";
import { ICommittee } from "../queries/get-committee-by-id.query";
import { CreateContributionInput } from "../input-types/create-contribution.input-type";
import { verifyDonor } from "./donor-verification.pipe";
import { IInstantIdConfig } from "../clients/lexis-nexis/lexis-nexis.client";
import { taskEither as te } from "fp-ts";
import { IComplianceResult, runComplianceCheck } from "./compliance-check.pipe";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { createContributionInputToDonorInput } from "../utils/model/create-contribution-input-to-donor-input.utils";
import { Plan } from "../utils/enums/plan.enum";
import { IDonor } from "../queries/search-donors.decoder";

export const runComplianceCheckOrSkip =
  (txnsTableName: string) =>
  (rulesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (contribInput: CreateContributionInput) =>
  (committee: ICommittee) =>
  (donor: IDonor): TaskEither<ApplicationError, IComplianceResult> =>
    committee.platformPlan === Plan.Policapital
      ? skipComplianceCheck(committee)(contribInput)(donor)
      : runComplianceCheck(txnsTableName)(rulesTableName)(dynamoDB)(
          contribInput
        )(committee)(donor);

const skipComplianceCheck =
  (committee: ICommittee) =>
  (contribInput: CreateContributionInput) =>
  (donor: IDonor): TaskEither<ApplicationError, IComplianceResult> =>
    te.of({
      createContributionInput: contribInput,
      committee,
      donor,
    });

export const runRulesEngine =
  (billableEventsTableName: string) =>
  (donorsTableName: string) =>
  (txnsTableName: string) =>
  (rulesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (instantIdConfig: IInstantIdConfig) =>
  (committee: ICommittee) =>
  (
    contribInput: CreateContributionInput
  ): TaskEither<ApplicationError, IComplianceResult> =>
    pipe(
      te.of(createContributionInputToDonorInput(contribInput)),
      te.chain(
        verifyDonor(billableEventsTableName)(donorsTableName)(dynamoDB)(
          instantIdConfig
        )(committee)
      ),
      te.chain(
        runComplianceCheckOrSkip(txnsTableName)(rulesTableName)(dynamoDB)(
          contribInput
        )(committee)
      )
    );
