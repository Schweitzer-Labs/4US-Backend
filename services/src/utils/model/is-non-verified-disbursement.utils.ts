import { ITransaction } from "../../model/transaction.type";
import { ApplicationError } from "../application-error";
import { TaskEither } from "fp-ts/TaskEither";
import { taskEither } from "fp-ts";
import { TransactionType } from "../enums/transaction-type.enum";
import { isBankVerifiedAndRuleUnverified } from "./is-non-verified-txn.utils";

export const isNotABankVerifiedRuleUnverifiedDisb = (
  t: ITransaction
): TaskEither<ApplicationError, ITransaction> => {
  if (
    t.transactionType === TransactionType.Disbursement &&
    !isBankVerifiedAndRuleUnverified(t.bankVerified, t.ruleVerified)
  ) {
    return taskEither.right(t);
  } else {
    return taskEither.left(
      new ApplicationError("Transaction is not an unverified disbursement.", {})
    );
  }
};
