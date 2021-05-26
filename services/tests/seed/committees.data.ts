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
    members: ["evan-piro"],
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
