import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../model/transaction.type";
import { pipe } from "fp-ts/function";
import { getTxnById } from "../utils/model/transaction/get-txn-by-id.utils";
import { taskEither, taskEither as te } from "fp-ts";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { reconcileTxnsByIds } from "../utils/model/transaction/reconcile-txn-by-id.utils";
import {
  deleteTxn,
  deleteTxnPipe,
} from "../utils/model/transaction/delete-txn.utils";
import { TransactionType } from "../utils/enums/transaction-type.enum";
import { getTxnsById } from "../utils/model/transaction/get-txns-by-id.utils";

// @Todo Add validation
export const reconcileTxnWithTxns =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string) =>
  (txnId: string) =>
  (selectedTxnIds: string[]) =>
    pipe(
      getTxnById(txnsTableName)(dynamoDB)(committeeId)(txnId),
      taskEither.chain(assertTxnAsBankTxn),
      taskEither.chain(
        assertTxnsCanBeReconciled(txnsTableName)(dynamoDB)(committeeId)(
          selectedTxnIds
        )
      ),
      taskEither.chain(
        useTxnToReconcileIds(txnsTableName)(dynamoDB)(selectedTxnIds)
      ),
      taskEither.chain(deleteTxnPipe(txnsTableName)(dynamoDB))
    );

export const assertTxnAsBankTxn = (
  txn: ITransaction
): TaskEither<ApplicationError, ITransaction> => {
  if (txn.bankVerified && !txn.ruleVerified) {
    return taskEither.right(txn);
  } else {
    return taskEither.left(
      new ApplicationError(
        "Bank transaction is not bank verified and rule unverified",
        {}
      )
    );
  }
};

export const assertTxnAsTxnType =
  (txnType: TransactionType) =>
  (txn: ITransaction): TaskEither<ApplicationError, ITransaction> => {
    if (
      txn.transactionType === txnType &&
      txn.bankVerified &&
      !txn.ruleVerified
    ) {
      return taskEither.right(txn);
    } else {
      return taskEither.left(
        new ApplicationError(
          "Bank transaction is not bank verified and rule unverified",
          {}
        )
      );
    }
  };

export const assertTxnsCanBeReconciled =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string) =>
  (selectedTxnIds: string[]) =>
  (bankTxn: ITransaction): TaskEither<ApplicationError, ITransaction> =>
    pipe(
      getTxnsById(txnsTableName)(dynamoDB)(committeeId)(selectedTxnIds),
      taskEither.chain(testTxns(bankTxn))
    );

const isUnreconciledTxn = (txn: ITransaction) =>
  !txn.bankVerified && txn.ruleVerified;

const testTxns =
  (bankTxn: ITransaction) =>
  (
    selectedTxns: ITransaction[]
  ): TaskEither<ApplicationError, ITransaction> => {
    let selectedSum = 0;

    for (const txn of selectedTxns) {
      selectedSum = txn.amount + selectedSum;
    }

    const areUnreconciledTxns = selectedTxns.reduce(
      (acc, val) => acc && isUnreconciledTxn(val),
      true
    );

    const haveSameTxnType = selectedTxns.every(
      (val) => val.transactionType === bankTxn.transactionType
    );

    // const haveSamePaymentType = selectedTxns.every(
    //   (val) => val.paymentMethod === bankTxn.paymentMethod
    // );

    if (
      selectedSum === bankTxn.amount &&
      // haveSamePaymentType &&
      areUnreconciledTxns &&
      haveSameTxnType
    ) {
      return taskEither.right(bankTxn);
    } else {
      return taskEither.left(
        new ApplicationError(
          "Selected transaction do not match bank transaction",
          {}
        )
      );
    }
  };

const useTxnToReconcileIds =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (txnIds: string[]) =>
  (txn: ITransaction): TaskEither<ApplicationError, ITransaction> =>
    pipe(
      tryCatch(
        () => reconcileTxnsByIds(txnsTableName)(dynamoDB)(txn)(txnIds),
        (e) => new ApplicationError("DDB Txn reconcile request failed", e)
      ),
      taskEither.map(() => txn)
    );
