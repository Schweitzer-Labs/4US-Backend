import { pipe } from "fp-ts/function";
import { runRulesEngine } from "./rules-engine.pipe";
import { taskEither as te } from "fp-ts";
import { processContribution } from "./process-contribution.pipe";
import { DynamoDB } from "aws-sdk";
import { ICommittee } from "../queries/get-committee-by-id.query";
import { Stripe } from "stripe";
import { ILexisNexisConfig } from "../clients/lexis-nexis/lexis-nexis.client";

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
  (createContributionInput) =>
    pipe(
      runRulesEngine(billableEventsTableName)(donorsTableName)(txnsTableName)(
        rulesTableName
      )(dynamoDB)(lnConfig)(committee)(createContributionInput),
      te.chain(
        processContribution(currentUser)(txnsTableName)(dynamoDB)(stripe)
      )
    );
