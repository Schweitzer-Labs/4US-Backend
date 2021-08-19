import { pipe } from "fp-ts/function";
import { getTxnById } from "../utils/model/get-txn-by-id.utils";
import { taskEither } from "fp-ts";
import { deleteTxnPipe } from "../utils/model/delete-txn.utils";
import { DynamoDB } from "aws-sdk";
import { ICommittee } from "../queries/get-committee-by-id.query";
import { TransactionArg } from "../args/transaction.arg";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { ITransaction } from "../queries/search-transactions.decoder";

export const deleteUnreconciledTxn =
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (txnArgs: TransactionArg): TaskEither<ApplicationError, ITransaction> =>
    pipe(
      getTxnById(txnTable)(ddb)(txnArgs.committeeId)(txnArgs.id),
      taskEither.chain(isReconciled),
      taskEither.chain(deleteTxnPipe(txnTable)(ddb))
    );

const isReconciled = (
  txn: ITransaction
): TaskEither<ApplicationError, ITransaction> =>
  txn.bankVerified === false && !txn.stripePaymentIntentId
    ? taskEither.right(txn)
    : taskEither.left(
        new ApplicationError(
          "Transaction is not unreconciled or unprocessed.",
          txn
        )
      );
