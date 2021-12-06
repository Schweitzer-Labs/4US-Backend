import { pipe } from "fp-ts/function";
import { IRunRuleConfig, runRulesEngine } from "./rules-engine.pipe";
import { taskEither as te } from "fp-ts";
import { processContribution } from "./process-contribution.pipe";
import { DynamoDB } from "aws-sdk";
import { Stripe } from "stripe";
import { ILexisNexisConfig } from "../clients/lexis-nexis/lexis-nexis.client";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { ITransaction } from "../model/transaction.type";
import { CreateContributionInput } from "../graphql/input-types/create-contribution.input-type";
import { ICommittee } from "../model/committee.type";

export const runRulesAndProcess =
  (runRuleConfig: IRunRuleConfig) =>
  (billableEventsTableName: string) =>
  (donorsTableName: string) =>
  (txnsTableName: string) =>
  (rulesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (stripe: Stripe) =>
  (lnConfig: ILexisNexisConfig) =>
  (currentUser: string) =>
  (committee: ICommittee) =>
  (
    createContributionInput: CreateContributionInput
  ): TaskEither<ApplicationError, ITransaction> =>
    pipe(
      runRulesEngine(runRuleConfig)(billableEventsTableName)(donorsTableName)(
        txnsTableName
      )(rulesTableName)(dynamoDB)(lnConfig)(committee)(createContributionInput),
      te.chain(
        processContribution(currentUser)(txnsTableName)(dynamoDB)(stripe)
      )
    );
