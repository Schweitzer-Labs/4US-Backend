import { ITransaction } from "../../model/transaction.type";

export interface IPayoutGroup {
  payoutId: string;
  amount: number;
  payoutDate: number;
  txns: ITransaction[];
}

export const groupTxnsByPayout = (txns: ITransaction[]): IPayoutGroup[] => {
  const txnsWithPayouts = txns.filter((txn) => !!txn.stripePayoutId);
  const payoutGroups = txnsWithPayouts.reduce((acc, txn) => {
    const currentAmount = acc[txn.stripePayoutId]?.amount || 0;

    acc[txn.stripePayoutId] = {
      amount: currentAmount + txn.amount,
      payoutDate: txn.stripeAutomaticPayoutEffectiveAtUtc,
      txns: [...(acc[txn.stripePayoutId]?.txns || []), txn],
    };
    return acc;
  }, {});

  const payoutGroupsList = Object.keys(payoutGroups).map((payoutId) => {
    return {
      payoutId,
      amount: payoutGroups[payoutId].amount,
      payoutDate: payoutGroups[payoutId].payoutDate,
      txns: payoutGroups[payoutId].txns,
    };
  });

  return payoutGroupsList.sort(function (a, b) {
    return b.payoutDate - a.payoutDate;
  });
};
