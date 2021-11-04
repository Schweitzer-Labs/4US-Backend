import { DynamoDB } from "aws-sdk";
import { deleteTxn } from "../../utils/model/delete-txn.utils";
import { putTransaction } from "../../utils/model/put-transaction.utils";
import {
  ITransaction,
  Transaction,
  Transactions,
} from "../../types/transaction.type";
import {
  groupTxnsByPayout,
  IPayoutGroup,
} from "../../utils/model/group-txns-by-payout.utils";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";
import { pipe } from "fp-ts/function";
import { searchTransactions } from "../../utils/model/search-transactions.query";
import { TransactionType } from "../../utils/enums/transaction-type.enum";
import { taskEither as te } from "fp-ts";
import { isPayout } from "../../pipes/reconcile-contributions.pipe";
import { decodeRawData } from "../../utils/decode-raw-data.util";

export const runReconcileOnCommittee =
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (committeeId: string): TaskEither<ApplicationError, ITransaction[]> =>
    pipe(
      searchTransactions(txnTable)(ddb)({
        committeeId,
        transactionType: TransactionType.Contribution,
      }),
      te.chain(recTxnsAndDecode(txnTable)(ddb))
    );

const withinADayOf =
  (d1: number) =>
  (d2: number): boolean =>
    Math.abs(d1 - d2) < 1000 * 60 * 60 * 24;

export const recTxnsAndDecode =
  (txnsTable: string) =>
  (ddb: DynamoDB) =>
  (txns: ITransaction[]): TaskEither<ApplicationError, ITransaction[]> =>
    pipe(
      te.tryCatch(
        () => recTxns(txnsTable)(ddb)(txns),
        (err) => new ApplicationError("Reconcile txns", err)
      ),
      te.chain(decodeRawData("Reconcile txns")(Transactions))
    );

export const recTxns =
  (txnsTable: string) =>
  (ddb: DynamoDB) =>
  async (txns: ITransaction[]): Promise<ITransaction[]> => {
    const matches = txns.filter((txn) => !!txn.stripePayoutId);

    const bankPayouts = txns.filter(
      (txn) => txn.bankVerified && !txn.ruleVerified && isPayout(txn)
    );

    const payoutGroups = groupTxnsByPayout(matches);

    const resTxns = [];
    for (const txn of bankPayouts) {
      const matches = payoutGroups.filter((payout) => {
        return (
          txn.amount === payout.amount &&
          withinADayOf(txn.paymentDate)(payout.payoutDate)
        );
      });
      if (matches.length === 1) {
        const match = matches[0];
        console.log("match found");
        console.log("bank txn", txn);
        await deleteTxn(txnsTable)(ddb)(txn);
        for (const pTxn of match.txns) {
          console.log("setting txns to verified");
          const recTxn = {
            ...pTxn,
            finicityTransactionData: txn.finicityTransactionData,
            finicityTransactionId: txn.finicityTransactionId,
            finicityCategory: txn.finicityCategory,
            finicityBestRepresentation: txn.finicityBestRepresentation,
            finicityPostedDate: txn.finicityPostedDate,
            finicityTransactionDate: txn.finicityTransactionDate,
            finicityNormalizedPayeeName: txn.finicityNormalizedPayeeName,
            finicityDescription: txn.finicityDescription,
            finicityPaymentMethod: txn.finicityPaymentMethod,
            bankVerified: true,
          };
          await putTransaction(txnsTable)(ddb)(recTxn);
          resTxns.push(recTxn);
        }
      }
    }
    return resTxns;
  };

export const runRec =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (payoutGroups: IPayoutGroup[]) =>
  async (bankPayouts: ITransaction[]) => {
    let reconCount = 0;
    for (const txn of bankPayouts) {
      const matches = payoutGroups.filter((payout) => {
        return (
          txn.amount === payout.amount &&
          withinADayOf(txn.paymentDate)(payout.payoutDate)
        );
      });
      if (matches.length === 1) {
        const match = matches[0];
        console.log("match found");
        console.log("bank txn", txn);
        await deleteTxn(txnsTableName)(dynamoDB)(txn);
        for (const pTxn of match.txns) {
          console.log("setting txns to verified");
          const recTxn = {
            ...pTxn,
            finicityTransactionData: txn.finicityTransactionData,
            finicityTransactionId: txn.finicityTransactionId,
            finicityCategory: txn.finicityCategory,
            finicityBestRepresentation: txn.finicityBestRepresentation,
            finicityPostedDate: txn.finicityPostedDate,
            finicityTransactionDate: txn.finicityTransactionDate,
            finicityNormalizedPayeeName: txn.finicityNormalizedPayeeName,
            finicityDescription: txn.finicityDescription,
            finicityPaymentMethod: txn.finicityPaymentMethod,
            bankVerified: true,
          };
          await putTransaction(txnsTableName)(dynamoDB)(recTxn);
          reconCount++;
        }
      }
    }
    console.log("rec count", reconCount);
  };
