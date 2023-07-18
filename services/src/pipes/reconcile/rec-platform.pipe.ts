import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";
import { ITransaction } from "../../model/transaction.type";
import { pipe } from "fp-ts/function";
import { DynamoDB } from "aws-sdk";
import {
  allSameValue,
  ILoadedRecInput,
  selectedTxnsAreUnreconciled,
  selectedTxnsTotalMatchesBank,
  useTxnToReconcileIds,
} from "./rec-utils.pipe";
import { taskEither } from "fp-ts";
import { deleteTxnPipe } from "../../utils/model/transaction/delete-txn.utils";

export const recPlatform =
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (input: ILoadedRecInput): TaskEither<ApplicationError, ITransaction> =>
    pipe(validate(input), taskEither.chain(runRec(txnTable)(ddb)));

const validate = (
  input: ILoadedRecInput
): TaskEither<ApplicationError, ILoadedRecInput> =>
  pipe(
    bankTxnIsUnreconciled(input),
    taskEither.chain(selectedTxnsAreUnreconciled),
    taskEither.chain(selectedTxnsTotalMatchesBank(calcTotal)),
    taskEither.chain(selectedTxnsHaveSameTxnType)
  );

const selectedTxnsHaveSameTxnType = (
  input: ILoadedRecInput
): TaskEither<ApplicationError, ILoadedRecInput> =>
  allSameValue(input.selectedTxns.map((txn) => txn.transactionType))
    ? taskEither.right(input)
    : taskEither.left(
        new ApplicationError(
          "Selected transactions must have the same transaction type",
          {}
        )
      );

const bankTxnIsUnreconciled = (
  input: ILoadedRecInput
): TaskEither<ApplicationError, ILoadedRecInput> =>
  input.bankTxn.bankVerified && !input.bankTxn.ruleVerified
    ? taskEither.right(input)
    : taskEither.left(
        new ApplicationError("Bank transaction must be unreconciled", {})
      );

const calcTotal = (txns: ITransaction[]): number =>
  txns.reduce((acc, val) => val.amount + acc, 0);

const runRec =
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (input: ILoadedRecInput): TaskEither<ApplicationError, ITransaction> =>
    pipe(
      useTxnToReconcileIds(txnTable)(ddb)(
        input.selectedTxns.map((txn) => txn.id)
      )(input.bankTxn),
      taskEither.chain(deleteTxnPipe(txnTable)(ddb))
    );
