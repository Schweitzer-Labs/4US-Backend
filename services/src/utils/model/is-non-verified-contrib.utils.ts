import { ITransaction } from "../../types/transaction.type";
import { ApplicationError } from "../application-error";
import { TaskEither } from "fp-ts/TaskEither";
import { taskEither } from "fp-ts";
import { isBankVerifiedAndRuleUnverified } from "./is-non-verified-txn.utils";
import { TransactionType } from "../enums/transaction-type.enum";

export const isRuleVerifiedBankUnverifiedContrib = (
  t: ITransaction
): TaskEither<ApplicationError, ITransaction> => {
  if (
    t.transactionType === TransactionType.Contribution &&
    !isBankVerifiedAndRuleUnverified(t.bankVerified, t.ruleVerified)
  ) {
    return taskEither.right(t);
  } else {
    return taskEither.left(
      new ApplicationError("Transaction is not an unverified disbursement.", {})
    );
  }
};
