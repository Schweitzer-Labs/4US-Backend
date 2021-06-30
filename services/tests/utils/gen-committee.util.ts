import { ICommittee } from "../../src/queries/get-committee-by-id.query";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import * as faker from "faker";
import { Plan } from "../../src/utils/enums/plan.enum";

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
  platformPlan?: string;
  finicityCustomerId?: string;
  finicityAccountId?: string;
  stripeAccount?: string;
  candidateFirstName?: string;
  candidateLastName?: string;
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
  platformPlan,
  finicityCustomerId,
  finicityAccountId,
  stripeAccount,
  candidateFirstName,
  candidateLastName,
}: IGenCommitteeConfig): ICommittee => {
  const randomLastName = faker.name.lastName();
  return {
    id: genTxnId(),
    committeeName: `Vote for ${candidateFirstName || randomLastName}`,
    candidateFirstName: candidateFirstName || faker.name.firstName(),
    candidateLastName: candidateLastName || randomLastName,
    stripeAccount: stripeAccount || "acct_1IjTcsRC8iiQex3V",
    members: [
      "evan-piro",
      "1643f48e-431c-4461-b2cb-b8fce4c939df",
      "6604131d-1982-4140-9d69-59206766151c",
    ],
    state,
    scope,
    officeType,
    party,
    race,
    district,
    county,
    ruleVersion,
    tzDatabaseName: tzDatabaseName || "America/New_York",
    emailAddresses: emailAddresses || "evan@schweitzerlabs.com",
    platformPlan: platformPlan || Plan.FourUs,
    finicityCustomerId,
    finicityAccountId,
    bankName: "chase",
  };
};
