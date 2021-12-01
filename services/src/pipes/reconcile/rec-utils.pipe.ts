import { ITransaction } from "../../model/transaction.type";
import { ICommittee } from "../../model/committee.type";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";
import { taskEither } from "fp-ts";
import { DynamoDB } from "aws-sdk";
import { ReconcileTxnInput } from "../../graphql/input-types/reconcile-txn.input-type";
import { pipe } from "fp-ts/function";
import { getTxnsById } from "../../utils/model/transaction/get-txns-by-id.utils";
import { getTxnById } from "../../utils/model/transaction/get-txn-by-id.utils";
import { reconcileTxnsByIds } from "../../utils/model/transaction/reconcile-txn-by-id.utils";

export type TotalCalculator = (txns: ITransaction[]) => number;

export interface ILoadedRecInput {
  selectedTxns: ITransaction[];
  bankTxn: ITransaction;
  committee: ICommittee;
}

export const allSameValue = (vals): boolean =>
  vals.length > 0 ? vals.every((val) => val === vals[0]) : false;

export const selectedTxnsAreUnreconciled = (
  input: ILoadedRecInput
): TaskEither<ApplicationError, ILoadedRecInput> =>
  input.selectedTxns.every((txn) => !txn.bankVerified && txn.ruleVerified)
    ? taskEither.right(input)
    : taskEither.left(
        new ApplicationError("Selected transactions must be unreconciled", {})
      );

export const selectedTxnsTotalMatchesBank =
  (calcTotal: TotalCalculator) =>
  (input: ILoadedRecInput): TaskEither<ApplicationError, ILoadedRecInput> =>
    calcTotal(input.selectedTxns) === input.bankTxn.amount
      ? taskEither.right(input)
      : taskEither.left(
          new ApplicationError(
            "Selected transactions must add up to bank transaction total",
            {}
          )
        );

export const loadRecInput =
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (recInput: ReconcileTxnInput) =>
  (com: ICommittee): TaskEither<ApplicationError, ILoadedRecInput> =>
    pipe(
      getTxnsById(txnTable)(ddb)(com.id)(recInput.selectedTransactions),
      taskEither.chain(
        toLoadedRecInput(txnTable)(ddb)(com)(recInput.bankTransaction)
      )
    );

export const useTxnToReconcileIds =
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

const toLoadedRecInput =
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (committee: ICommittee) =>
  (bankTxnId: string) =>
  (
    selectedTxns: ITransaction[]
  ): TaskEither<ApplicationError, ILoadedRecInput> =>
    pipe(
      getTxnById(txnTable)(ddb)(committee.id)(bankTxnId),
      taskEither.map((bankTxn) => ({
        bankTxn,
        selectedTxns,
        committee,
      }))
    );
