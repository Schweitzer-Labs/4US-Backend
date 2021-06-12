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
}: IGenCommitteeConfig): ICommittee => {
  const candidateFirstName = faker.name.firstName();
  const candidateLastName = faker.name.lastName();
  return {
    id: genTxnId(),
    committeeName: `Vote for ${candidateFirstName}`,
    candidateFirstName,
    candidateLastName,
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
    tzDatabaseName: tzDatabaseName || "America/New_York",
    emailAddresses: emailAddresses || "evan@schweitzerlabs.com",
    platformPlan: platformPlan || Plan.FourUs,
    finicityCustomerId,
    finicityAccountId,
  };
};
