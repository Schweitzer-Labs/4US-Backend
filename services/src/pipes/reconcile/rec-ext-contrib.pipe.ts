import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";
import { ITransaction } from "../../model/transaction.type";
import { pipe } from "fp-ts/function";
import { DynamoDB } from "aws-sdk";
import {
  allSameValue,
  ILoadedRecInput,
  selectedTxnsTotalMatchesBank,
  TotalCalculator,
  useTxnToReconcileIds,
} from "./rec-utils.pipe";
import { taskEither } from "fp-ts";
import { getTxnsById } from "../../utils/model/transaction/get-txns-by-id.utils";
import { getFeeTxnByExternalId } from "../../utils/model/transaction/get-txn-by-external-txn-id.utils";
import * as Array from "fp-ts/Array";
import { getTxnById } from "../../utils/model/transaction/get-txn-by-id.utils";
import { deleteTxnPipe } from "../../utils/model/transaction/delete-txn.utils";

export const recExtContrib =
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (input: ILoadedRecInput): TaskEither<ApplicationError, ITransaction> =>
    pipe(validate(input), taskEither.chain(runRec(txnTable)(ddb)));

const runRec =
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (input: ILoadedRecInput): TaskEither<ApplicationError, ITransaction> =>
    pipe(
      selectedTxnsToFeeTxns(txnTable)(ddb)(input),
      taskEither.map((feeTxns) => feeTxns.map((feeTxn) => feeTxn.id)),
      taskEither.chain((feeTxnIds) =>
        pipe(
          useTxnToReconcileIds(txnTable)(ddb)(
            input.selectedTxns.map((txn) => txn.id).concat(feeTxnIds)
          )(input.bankTxn),
          taskEither.chain(deleteTxnPipe(txnTable)(ddb))
        )
      )
    );

const selectedTxnsToFeeTxns =
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (input: ILoadedRecInput): TaskEither<ApplicationError, ITransaction[]> =>
    Array.traverse(taskEither.ApplicativeSeq)(
      getFeeTxnByExternalId(txnTable)(ddb)(input.committee.id)
    )(input.selectedTxns.map((txn) => txn.externalTransactionId));

const validate = (
  input: ILoadedRecInput
): TaskEither<ApplicationError, ILoadedRecInput> =>
  pipe(
    selectedTxnsHaveSamePayoutId(input),
    taskEither.chain(selectedTxnsAreUnreconciled),
    taskEither.chain(selectedTxnsTotalMatchesBank(calcTotal))
  );

const selectedTxnsHaveSamePayoutId = (
  input: ILoadedRecInput
): TaskEither<ApplicationError, ILoadedRecInput> =>
  allSameValue(input.selectedTxns.map((txn) => txn.externalTransactionPayoutId))
    ? taskEither.right(input)
    : taskEither.left(
        new ApplicationError(
          "Selected transactions must share the same payout ID",
          {}
        )
      );

const selectedTxnsAreUnreconciled = (
  input: ILoadedRecInput
): TaskEither<ApplicationError, ILoadedRecInput> =>
  input.selectedTxns.every((txn) => !txn.bankVerified && txn.ruleVerified)
    ? taskEither.right(input)
    : taskEither.left(
        new ApplicationError("Selected transactions must be unreconciled", {})
      );

const calcTotal: TotalCalculator = (txns: ITransaction[]): number =>
  txns.reduce((acc, val) => val.amount - val.processorFeeData.amount + acc, 0);
