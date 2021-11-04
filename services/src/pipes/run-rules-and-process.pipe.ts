import { pipe } from "fp-ts/function";
import { runRulesEngine } from "./rules-engine.pipe";
import { taskEither as te } from "fp-ts";
import { processContribution } from "./process-contribution.pipe";
import { DynamoDB } from "aws-sdk";
import { Stripe } from "stripe";
import { ILexisNexisConfig } from "../clients/lexis-nexis/lexis-nexis.client";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { ITransaction } from "../queries/search-transactions.decoder";
import { CreateContributionInput } from "../graphql/input-types/create-contribution.input-type";
import { ICommittee } from "../types/committee.type";

export const runRulesAndProcess =
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
      runRulesEngine(billableEventsTableName)(donorsTableName)(txnsTableName)(
        rulesTableName
      )(dynamoDB)(lnConfig)(committee)(createContributionInput),
      te.chain(
        processContribution(currentUser)(txnsTableName)(dynamoDB)(stripe)
      )
    );
