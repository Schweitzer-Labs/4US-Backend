export const getCommitteeById = (committeeId: string) => {};

interface Committee {
  id: string;
  committeeName: string;
  candidateFirstName: string;
  candidateMiddleName: string;
  candidateLastName: string;
  state?: string;
  scope?: string;
  officeType?: string;
  party?: string;
  race?: string;
  district?: string;
  county?: string;
  ruleVersion: string;
  chainId: string;
  plaidAccessToken: string;
  plaidRefreshToken?: string;
  stripeAccessToken: string;
  stripeUserId: string;
}
