import * as faker from "faker";
import { ICommittee } from "../../queries/get-committee-by-id.query";
import { genTxnId } from "../../utils/gen-txn-id.utils";
import { qaUsers } from "../data/qa-users.data";
import { Plan } from "../../utils/enums/plan.enum";

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
  efsElectionId?: number;
  efsFilerId?: number;
  members?: string[];
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
  candidateFirstName,
  candidateLastName,
  stripeAccount,
  efsElectionId,
  efsFilerId,
  members,
}: IGenCommitteeConfig): ICommittee => {
  const randomFirstName = faker.name.firstName();
  const randomLastName = faker.name.lastName();
  return {
    id: genTxnId(),
    committeeName: `Vote for ${candidateFirstName || randomLastName}`,
    candidateFirstName: candidateFirstName || randomFirstName,
    candidateLastName: candidateLastName || randomLastName,
    stripeAccount: stripeAccount || "acct_1IjTcsRC8iiQex3V",
    members: members || qaUsers,
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
    efsElectionId: efsElectionId || faker.datatype.number(5000000),
    efsFilerId: efsFilerId || faker.datatype.number(5000000),
  };
};