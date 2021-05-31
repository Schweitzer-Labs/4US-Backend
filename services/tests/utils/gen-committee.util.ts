import { ICommittee } from "../../src/queries/get-committee-by-id.query";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import * as faker from "faker";

interface IGenCommitteeConfig {
  state?: string;
  scope?: string;
  officeType?: string;
  party?: string;
  race?: string;
  district?: string;
  county?: string;
  ruleVersion?: string;
  tzDatabaseName?: string;
  emailAddresses?: string;
  plan?: string;
}

export const genCommittee = ({
  state,
  scope,
  officeType,
  party,
  race,
  district,
  county,
  ruleVersion,
  tzDatabaseName,
  emailAddresses,
  plan,
}: IGenCommitteeConfig): ICommittee => ({
  id: genTxnId(),
  committeeName: `Vote for ${faker.name.lastName()}`,
  candidateFirstName: faker.name.firstName(),
  candidateLastName: faker.name.lastName(),
  stripeAccount: "acct_1IjTcsRC8iiQex3V",
  members: ["evan-piro", "5e41bd77-eae7-4b2d-8d20-a05bef22c4e2"],
  state,
  scope,
  officeType,
  party,
  race,
  district,
  county,
  ruleVersion,
  tzDatabaseName,
  emailAddresses: emailAddresses || "evan@schweitzerlabs.com",
  plan,
});
