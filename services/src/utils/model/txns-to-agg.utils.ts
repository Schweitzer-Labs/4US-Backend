import { ITransaction } from "../../queries/search-transactions.decoder";
import { TransactionType } from "../enums/transaction-type.enum";
import { Direction } from "../enums/direction.enum";
import { Field } from "type-graphql";

export interface IAggregations {
  balance: number;
  totalRaised: number;
  totalSpent: number;
  totalDonors: number;
  totalTransactions: number;
  totalContributionsInProcessing: number;
  totalDisbursementsInProcessing: number;
  needsReviewCount: number;
}

export const txnsToAgg = (txns: ITransaction[]): IAggregations => {
  const init: any = {
    balance: 0,
    totalRaised: 0,
    totalSpent: 0,
    totalDonors: 0,
    totalTransactions: txns.length,
    totalContributionsInProcessing: 0,
    totalDisbursementsInProcessing: 0,
    needsReviewCount: 0,
    donorMap: {},
  };

  const aggs: any = txns.reduce((acc: any, txn) => {
    // Total Raised
    if (txn.transactionType === TransactionType.Contribution) {
      if (txn.bankVerified) {
        acc.totalRaised = acc.totalRaised + txn.amount;
        acc.balance = acc.balance + txn.amount;
      } else {
        acc.totalContributionsInProcessing =
          acc.totalContributionsInProcessing + txn.amount;
      }
      if (txn.donorId) {
        acc.donorMap[txn.donorId] = true;
      }
    } else if (txn.direction === Direction.In) {
      if (txn.bankVerified) {
        acc.balance = acc.balance + txn.amount;
      }
    }
    /// Total Spent
    if (txn.direction === Direction.Out) {
      if (txn.bankVerified) {
        acc.totalSpent = acc.totalSpent + txn.amount;
        acc.balance = acc.balance - txn.amount;
      } else {
        acc.totalDisbursementsInProcessing =
          acc.totalDisbursementsInProcessing + txn.amount;
      }
    }
    if (!txn.ruleVerified || !txn.bankVerified) {
      acc.needsReviewCount++;
    }

    return acc;
  }, init);

  aggs.totalDonors = Object.keys(aggs.donorMap).length;

  return aggs;
};
