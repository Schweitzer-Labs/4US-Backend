import * as t from "io-ts";
import { ActBlueAPICredentials } from "../clients/actblue/actblue.decoders";

const CommitteeRequired = t.type({
  id: t.string,
  committeeName: t.string,
  candidateFirstName: t.string,
  candidateLastName: t.string,
  stripeAccount: t.string,
  members: t.array(t.string),
  tzDatabaseName: t.string,
  platformPlan: t.string,
});

const CommitteeOptional = t.partial({
  candidateMiddleName: t.string,
  state: t.string,
  scope: t.string,
  officeType: t.string,
  party: t.string,
  race: t.string,
  district: t.string,
  county: t.string,
  bankName: t.string,
  ruleVersion: t.string,
  finicityCustomerId: t.string,
  finicityAccountId: t.string,
  chainId: t.string,
  emailAddresses: t.string,
  employmentStatus: t.string,
  actBlueAccountId: t.string,
  actBlueAPICredentials: ActBlueAPICredentials,
  efsFilerId: t.number,
  efsElectionId: t.number,
  blockchainMetadata: t.unknown,
});

export const Committee = t.intersection([CommitteeRequired, CommitteeOptional]);

export const Committees = t.array(Committee);

export type ICommittee = t.TypeOf<typeof Committee>;
