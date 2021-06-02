import { ITransaction } from "../../queries/search-transactions.decoder";
import { ApplicationError } from "../application-error";
import { TaskEither } from "fp-ts/TaskEither";
import { taskEither } from "fp-ts";
import { TransactionType } from "../enums/transaction-type.enum";

export const isNonVerifiedDisbursement = (
  t: ITransaction
): TaskEither<ApplicationError, ITransaction> => {
  if (
    t.transactionType === TransactionType.DISBURSEMENT &&
    t.bankVerified &&
    !t.ruleVerified
  ) {
    return taskEither.right(t);
  } else {
    return taskEither.left(
      new ApplicationError("Transaction is not an unverified disbursement.", {})
    );
  }
};
