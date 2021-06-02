import * as t from "io-ts";
import { ICommittee } from "../../src/queries/get-committee-by-id.query";

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
    plan: "policapital",
    members: ["evan-piro", "5e41bd77-eae7-4b2d-8d20-a05bef22c4e2"],
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
    tzDatabaseName: "America/New_York",
    plan: "policapital",
    members: ["james-martin"],
  },
];
