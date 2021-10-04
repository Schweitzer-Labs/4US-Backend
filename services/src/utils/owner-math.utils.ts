import { IOwner, ITransaction } from "../queries/search-transactions.decoder";
import { flow } from "fp-ts/function";
import { ApplicationError } from "./application-error";

const toIntOrThrow = (owner: IOwner): number => {
  const num = parseInt(owner.percentOwnership);
  if (isNaN(num))
    throw new ApplicationError("Percent ownership is not a number", owner);
  return num;
};

export const calcAmount = (origAmount: number) =>
  flow(
    toIntOrThrow,
    (n) => n * origAmount,
    (n) => n / 100,
    Math.round
  );

export const prepareOwners = (txn: ITransaction): IOwner[] => {
  const stagedOwners = txn.owners.map((owner) => ({
    ...owner,
    attributedAmount: calcAmount(txn.amount)(owner),
  }));

  const calcTotal = stagedOwners.reduce(
    (acc, owner) => acc + owner.attributedAmount,
    0
  );

  if (calcTotal === txn.amount) {
    return stagedOwners;
  } else {
    const diff = txn.amount - calcTotal;
    console.log("amount does not match", diff);
    stagedOwners[0] = {
      ...stagedOwners[0],
      attributedAmount: stagedOwners[0].attributedAmount + diff,
    };
    return stagedOwners;
  }
};
