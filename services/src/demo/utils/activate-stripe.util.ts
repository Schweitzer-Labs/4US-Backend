import { ICommittee } from "../../queries/get-committee-by-id.query";

export const activateStripe = (com: ICommittee): ICommittee => ({
  ...com,
  stripeAccount: "acct_1IjTcsRC8iiQex3V",
});
