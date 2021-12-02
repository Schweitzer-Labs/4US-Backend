import { ITransaction } from "../../src/model/transaction.type";

export const calcExtPayoutSet = (txns: ITransaction[]): number =>
  txns.reduce((acc, txn) => {
    let amount = txn.amount - txn.processorFeeData.amount;
    return amount + acc;
  }, 0);
