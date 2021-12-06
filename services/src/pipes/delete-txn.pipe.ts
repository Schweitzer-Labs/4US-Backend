import { pipe } from "fp-ts/function";
import { getTxnById } from "../utils/model/transaction/get-txn-by-id.utils";
import { taskEither } from "fp-ts";
import { deleteTxnPipe } from "../utils/model/transaction/delete-txn.utils";
import { DynamoDB } from "aws-sdk";
import { TransactionArg } from "../graphql/args/transaction.arg";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { ITransaction } from "../model/transaction.type";
import { ExternalSource } from "../utils/enums/source.enum";
import { enumToValues } from "../utils/enums/poly.util";

export const deleteUnreconciledTxn =
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (txnArgs: TransactionArg): TaskEither<ApplicationError, ITransaction> =>
    pipe(
      getTxnById(txnTable)(ddb)(txnArgs.committeeId)(txnArgs.id),
      taskEither.chain(isNotReconciled),
      taskEither.chain(isNotExternalContrib),
      taskEither.chain(deleteTxnPipe(txnTable)(ddb))
    );

const isNotReconciled = (
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

const isNotExternalContrib = (
  txn: ITransaction
): TaskEither<ApplicationError, ITransaction> =>
  enumToValues(ExternalSource).includes(txn.source)
    ? taskEither.left(
        new ApplicationError(
          `${txn.source} transactions cannot be deleted.`,
          txn
        )
      )
    : taskEither.right(txn);
