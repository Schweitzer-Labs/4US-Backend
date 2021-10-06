import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";
import { taskEither } from "fp-ts";
import { ReconcileTxnInput } from "../input-types/reconcile-txn.input-type";

export const validateReconcileInput = (
  r: ReconcileTxnInput
): TaskEither<ApplicationError, boolean> => {
  if (r.selectedTransactions.length === 0)
    return taskEither.left(
      new ApplicationError("Selected transactions list cannot be empty", r)
    );
  return taskEither.right(true);
};
