import * as t from "io-ts";
import { ICommittee } from "../../src/queries/get-committee-by-id.query";
import { Plan } from "../../src/utils/enums/plan.enum";

export const committeesData: ICommittee[] = [
  {
    // required
    id: "pat-miller",
    committeeName: "Patrick for Freedom",
    candidateFirstName: "Patrick",
    candidateLastName: "Miller",
    stripeAccount: "acct_1IjTcsRC8iiQex3V",
    emailAddresses: "evan@schweitzerlabs.com,seemant@schweitzerlabs.com",
    tzDatabaseName: "America/New_York",
    platformPlan: Plan.FourUs,
    members: ["evan-piro", "e1b4b8db-a24d-4b5c-af17-0b5ee6ef6b67"],
    state: "ny",
    scope: "state",
    party: "democrat",
    race: "general",
    district: "35",
    county: "",
    officeType: "assembly",
    ruleVersion: "nyboe-2020",
  },
  {
    // required
    id: "angel-cruz",
    committeeName: "Angel Cruz for Bronx Judge",
    candidateFirstName: "Angel",
    candidateLastName: "Cruz",
    stripeAccount: "acct_1IjTcsRC8iiQex3V",
    emailAddresses: "evan@schweitzerlabs.com,seemant@schweitzerlabs.com",
    platformPlan: Plan.FourUs,
    members: ["james-martin", "5e41bd77-eae7-4b2d-8d20-a05bef22c4e2"],
    tzDatabaseName: "America/New_York",
    state: "ny",
    scope: "state",
    party: "democrat",
    race: "general",
    district: "35",
    county: "",
    officeType: "assembly",
    ruleVersion: "nyboe-2020",
  },
  {
    // required
    id: "john-safford",
    committeeName: "John Safford for Supervisor",
    candidateFirstName: "John",
    candidateLastName: "Safford",
    stripeAccount: "acct_1IjTcsRC8iiQex3V",
    emailAddresses: "evan@schweitzerlabs.com,seemant@schweitzerlabs.com",
    platformPlan: Plan.Policapital,
    members: ["james-martin", "5e41bd77-eae7-4b2d-8d20-a05bef22c4e2"],
    tzDatabaseName: "America/New_York",
    state: "ny",
    scope: "state",
    party: "democrat",
    race: "general",
    district: "35",
    county: "",
    officeType: "assembly",
    ruleVersion: "nyboe-2020",
  },
];
