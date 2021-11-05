import { IRule } from "../../../model/rule.type";
import { IDonor } from "../../../model/donor.type";
import { ICommittee } from "../../../model/committee.type";

export const committeeAndDonorToRuleCode =
  (c: ICommittee) =>
  (d: IDonor): string =>
    [
      c.state,
      c.scope,
      c.party,
      c.race,
      c.district,
      c.county,
      c.officeType,
      c.ruleVersion,
      d.entityType,
    ].reduce((acc, val) => acc + `[${(val || "").toLowerCase()}]`, "");

export const ruleToRuleCode = (r: IRule): string =>
  [
    r.state,
    r.scope,
    r.party,
    r.race,
    r.district,
    r.county,
    r.officeType,
    r.ruleVersion,
    r.entityType,
  ].reduce((acc, val) => acc + `[${(val || "").toLowerCase()}]`, "");
