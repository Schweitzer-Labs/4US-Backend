import * as t from "io-ts";
import { fromEnum } from "../utils/from-enum.utils";
import { EmploymentStatus } from "../utils/enums/employment-status";
export enum Verdict {
  Passing = "Accepted",
  ExceedsLimit = "ExceedsLimit",
}

const RuleRequired = t.type({
  code: t.string,
});

const RuleOptional = t.partial({
  state: t.string,
  scope: t.string,
  party: t.string,
  race: t.string,
  district: t.string,
  county: t.string,
  officeType: t.string,
  ruleVersion: t.string,
  entityType: t.string,
  aggregateDuration: t.string,
  fields: t.array(t.string),
  limit: t.number,
});

export const Rule = t.intersection([RuleRequired, RuleOptional]);
export const Rules = t.array(Rule);
export type IRule = t.TypeOf<typeof Rule>;

export const RuleResult = t.type({
  balanceAtRuleRun: t.number,
  remaining: t.number,
  rule: Rule,
  verdict: fromEnum<Verdict>("Verdict", Verdict),
});

export type IRuleResult = t.TypeOf<typeof RuleResult>;

export enum AggregateDuration {
  AGGREGATE_LIMIT = "aggregate_limit",
  CALENDAR_YEAR = "calendar_year",
}
