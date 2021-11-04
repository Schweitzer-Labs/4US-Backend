import { ITransaction } from "../../types/transaction.type";
import { ApplicationError } from "../application-error";
import { TaskEither } from "fp-ts/TaskEither";
import { taskEither } from "fp-ts";

export const isBankVerifiedAndRuleUnverified = (
  bankVerified: boolean,
  ruleVerified: boolean
) => bankVerified && !ruleVerified;

export const isNotABankVerifiedRuleUnverifiedTxn = (
  t: ITransaction
): TaskEither<ApplicationError, ITransaction> => {
  if (!isBankVerifiedAndRuleUnverified(t.bankVerified, t.ruleVerified)) {
    return taskEither.right(t);
  } else {
    return taskEither.left(
      new ApplicationError("Transaction is not an unverified disbursement.", {})
    );
  }
};
