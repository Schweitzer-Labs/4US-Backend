import { ITransaction } from "../../types/transaction.type";
import { TransactionType } from "../enums/transaction-type.enum";
import { Direction } from "../enums/direction.enum";
import { Field } from "type-graphql";
import { PaymentMethod } from "../enums/payment-method.enum";
import { IAggs } from "../../types/aggs.type";

export const txnsToAgg =
  (committeeId: string) =>
  (txns: ITransaction[]): IAggs => {
    const init = {
      balance: 0,
      totalRaised: 0,
      totalSpent: 0,
      totalDonors: 0,
      totalTransactions: txns.length,
      totalContributionsInProcessing: 0,
      totalDisbursementsInProcessing: 0,
      needsReviewCount: 0,
      donorMap: {},
      committeeId,
    };

    const aggs: any = txns.reduce((acc: any, txn) => {
      // Total Raised
      if (txn.transactionType === TransactionType.Contribution) {
        if (txn.bankVerified) {
          acc.totalRaised = acc.totalRaised + txn.amount;
          if (txn.paymentMethod !== PaymentMethod.InKind) {
            acc.balance = acc.balance + txn.amount;
          }
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

    return removeDonorMap(aggs);
  };

const removeDonorMap = ({ donorMap, ...rest }: any) => rest;
