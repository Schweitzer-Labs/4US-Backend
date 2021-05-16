import * as t from "io-ts";
import { ddbString, ddbStringList } from "../ddb.utils";

const DDBCommittee = t.type({
  committeeName: ddbString,
  id: ddbString,
  stripeUserId: ddbString,
  emailAddresses: ddbStringList,
});

export const DDBCommitteeRes = t.type({
  Items: t.array(DDBCommittee),
});

export type DDBCommitteeRes = t.TypeOf<typeof DDBCommitteeRes>;

export interface Committee {
  committeeName: string;
  id: string;
  stripeUserId: string;
  emailAddresses: string[];
}
