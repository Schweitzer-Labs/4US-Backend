import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../queries/search-transactions.decoder";
import { pipe } from "fp-ts/function";
import { getTxnById } from "../utils/model/get-txn-by-id.utils";
import { taskEither, taskEither as te } from "fp-ts";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { reconcileTxnsByIds } from "../utils/model/reconcile-txn-by-id.utils";
import { deleteTxn, deleteTxnPipe } from "../utils/model/delete-txn.utils";

// @Todo Add validation
export const reconcileDisbursement =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string) =>
  (txnId: string) =>
  (selectedTxnIds: string[]) =>
    pipe(
      getTxnById(txnsTableName)(dynamoDB)(committeeId)(txnId),
      taskEither.chain(
        useTxnToReconcileIds(txnsTableName)(dynamoDB)(selectedTxnIds)
      ),
      taskEither.chain(deleteTxnPipe(txnsTableName)(dynamoDB))
    );

const useTxnToReconcileIds =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (txnIds: string[]) =>
  (txn: ITransaction): TaskEither<ApplicationError, ITransaction> =>
    pipe(
      tryCatch(
        () => reconcileTxnsByIds(txnsTableName)(dynamoDB)(txn)(txnIds),
        (e) =>
          new ApplicationError("DDB Disbursement reconcile request failed", e)
      ),
      taskEither.map(() => txn)
    );
