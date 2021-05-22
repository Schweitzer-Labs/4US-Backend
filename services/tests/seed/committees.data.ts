import * as t from "io-ts";
import { ICommittee } from "../../src/queries/get-committee-by-id.query";

export const committeesData: ICommittee[] = [
  {
    // required
    id: "907b427a-f8a9-450b-9d3c-33d8ec4a4cc4",
    committeeName: "Patrick for Freedom",
    candidateFirstName: "Patrick",
    candidateLastName: "Newcastle",
    stripeAccount: "acct_1IjTcsRC8iiQex3V",
    emailAddresses: "evan@schweitzerlabs.com,seemant@schweitzerlabs.com",
    tzDatabaseName: "America/New_York",
    plan: "policapital",
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
  },
];
