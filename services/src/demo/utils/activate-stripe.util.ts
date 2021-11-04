import { ICommittee } from "../../model/committee.type";

export const activateStripe = (com: ICommittee): ICommittee => ({
  ...com,
  stripeAccount: "acct_1IjTcsRC8iiQex3V",
});
