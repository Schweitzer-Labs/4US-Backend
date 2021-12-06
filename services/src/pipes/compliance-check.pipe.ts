import { IDonor } from "../model/donor.type";
import { pipe } from "fp-ts/function";
import { committeeAndDonorToRule } from "../utils/model/rule/get-rule.query";
import { DynamoDB } from "aws-sdk";
import { taskEither as te } from "fp-ts";
import { ApplicationError } from "../utils/application-error";
import { TaskEither } from "fp-ts/TaskEither";
import { searchTransactions } from "../utils/model/transaction/search-transactions.query";
import { AggregateDuration, IRule } from "../model/rule.type";
import { EntityType } from "../utils/enums/entity-type.enum";
import { StatusCodes } from "http-status-codes";
import { CreateContributionInput } from "../graphql/input-types/create-contribution.input-type";
import { ITransaction } from "../model/transaction.type";
import { millisToYear } from "../utils/time.utils";
import { ICommittee } from "../model/committee.type";

const committeeDonorAndRuleToBalance =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (ccInput: CreateContributionInput) =>
  (committee: ICommittee) =>
  (donor: IDonor) =>
  (rule: IRule): TaskEither<ApplicationError, number> => {
    const query =
      donor.entityType === EntityType.Fam
        ? { entityType: donor.entityType }
        : { donorId: donor.id };
    return pipe(
      searchTransactions(txnsTableName)(dynamoDB)({
        committeeId: committee.id,
        ...query,
      }),
      te.map(aggregateBalance(ccInput)(rule))
    );
  };

const aggregateBalance =
  (ccInput: CreateContributionInput) =>
  (rule: IRule) =>
  (txns: ITransaction[]): number => {
    const filter =
      rule.aggregateDuration === AggregateDuration.CALENDAR_YEAR
        ? (txn: ITransaction) =>
            millisToYear(txn.paymentDate) === millisToYear(ccInput.paymentDate)
        : () => true;

    return txns.filter(filter).reduce((acc, { amount }) => amount + acc, 0);
  };

interface IRuleResult {
  balanceAtRuleRun: number;
  remaining: number;
  rule: IRule;
  verdict: Verdict;
}

const runRule =
  (allowInvalid: boolean) =>
  (attemptedAmount: number) =>
  (rule: IRule) =>
  (balanceAtRuleRun: number): TaskEither<ApplicationError, IRuleResult> => {
    const remaining = rule.limit - balanceAtRuleRun;
    const exceedsLimit = balanceAtRuleRun + attemptedAmount > rule.limit;
    if (exceedsLimit)
      if (allowInvalid)
        // @ToDo make remaining update calc more explicit
        return te.right({
          balanceAtRuleRun,
          remaining,
          rule,
          verdict: Verdict.ExceedsLimit,
        });
      else
        return te.left(
          new ApplicationError(
            "Excess contribution attempted",
            { remaining },
            StatusCodes.UNAUTHORIZED
          )
        );
    else
      return te.right({
        balanceAtRuleRun,
        remaining,
        rule,
        verdict: Verdict.Passing,
      });
  };

const committeeDonorAndRuleToDetermination =
  (allowInvalid: boolean) =>
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (ccInput: CreateContributionInput) =>
  (committee: ICommittee) =>
  (donor: IDonor) =>
  (rule: IRule): TaskEither<ApplicationError, IRuleResult> =>
    pipe(
      committeeDonorAndRuleToBalance(txnsTableName)(dynamoDB)(ccInput)(
        committee
      )(donor)(rule),
      te.chain(runRule(allowInvalid)(ccInput.amount)(rule))
    );

export enum Verdict {
  Passing = "Accepted",
  ExceedsLimit = "ExceedsLimit",
}

export interface IComplianceResult {
  donor?: IDonor;
  committee: ICommittee;
  createContributionInput: CreateContributionInput;
  rule?: IRule;
  balanceAtRuleRun?: number;
  remaining?: number;
  verdict?: Verdict;
}

export const runComplianceCheck =
  (allowInvalid: boolean) =>
  (txnsTableName: string) =>
  (rulesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (createContributionInput: CreateContributionInput) =>
  (committee: ICommittee) =>
  (donor: IDonor): TaskEither<ApplicationError, IComplianceResult> => {
    return pipe(
      committeeAndDonorToRule(rulesTableName)(dynamoDB)(committee)(donor),
      te.chain(
        committeeDonorAndRuleToDetermination(allowInvalid)(txnsTableName)(
          dynamoDB
        )(createContributionInput)(committee)(donor)
      ),
      te.map(({ rule, balanceAtRuleRun, remaining, verdict }) => ({
        donor,
        committee,
        rule,
        balanceAtRuleRun,
        remaining,
        createContributionInput,
        verdict,
      }))
    );
  };
