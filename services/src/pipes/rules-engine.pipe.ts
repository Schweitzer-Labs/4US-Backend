import { pipe } from "fp-ts/function";
import { DynamoDB } from "aws-sdk";
import { verifyDonor } from "./donor-verification.pipe";
import { taskEither as te } from "fp-ts";
import { IComplianceResult, runComplianceCheck } from "./compliance-check.pipe";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { createContributionInputToDonorInput } from "../utils/model/donor/create-contribution-input-to-donor-input.utils";
import { Plan } from "../utils/enums/plan.enum";
import { IDonor } from "../model/donor.type";
import { ILexisNexisConfig } from "../clients/lexis-nexis/lexis-nexis.client";
import { CreateContributionInput } from "../graphql/input-types/create-contribution.input-type";
import { ICommittee } from "../model/committee.type";

export interface IRunRuleConfig {
  allowInvalid: boolean;
  idVerifyEnabled: boolean;
}

export const runComplianceCheckOrSkip =
  (runRulConfig: IRunRuleConfig) =>
  (txnsTableName: string) =>
  (rulesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (contribInput: CreateContributionInput) =>
  (committee: ICommittee) =>
  (donor: IDonor): TaskEither<ApplicationError, IComplianceResult> =>
    committee.platformPlan === Plan.Policapital
      ? skipComplianceCheck(committee)(contribInput)(donor)
      : runComplianceCheck(runRulConfig.allowInvalid)(txnsTableName)(
          rulesTableName
        )(dynamoDB)(contribInput)(committee)(donor);

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
  (runRuleConfig: IRunRuleConfig) =>
  (billableEventsTableName: string) =>
  (donorsTableName: string) =>
  (txnsTableName: string) =>
  (rulesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (lnConfig: ILexisNexisConfig) =>
  (committee: ICommittee) =>
  (
    contribInput: CreateContributionInput
  ): TaskEither<ApplicationError, IComplianceResult> =>
    pipe(
      te.of(createContributionInputToDonorInput(contribInput)),
      te.chain(
        verifyDonor(runRuleConfig.idVerifyEnabled)(billableEventsTableName)(
          donorsTableName
        )(dynamoDB)(lnConfig)(committee)
      ),
      te.chain(
        runComplianceCheckOrSkip(runRuleConfig)(txnsTableName)(rulesTableName)(
          dynamoDB
        )(contribInput)(committee)
      )
    );
