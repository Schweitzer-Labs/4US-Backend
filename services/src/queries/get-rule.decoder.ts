import * as t from "io-ts";

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
