import * as D from "io-ts/Decoder";

const ddbString = D.struct({
  S: D.string,
});

const ddbStringList = D.struct({
  SS: D.array(D.string),
});

const DDBCommittee = D.struct({
  committeeName: ddbString,
  id: ddbString,
  stripeUserId: ddbString,
  emailAddresses: ddbStringList,
});

export const DDBCommitteeRes = D.struct({
  Items: D.array(DDBCommittee),
});

export type DDBCommitteeRes = D.TypeOf<typeof DDBCommitteeRes>;

export interface Committee {
  committeeName: string;
  id: string;
  stripeUserId: string;
  emailAddresses: string[];
}
