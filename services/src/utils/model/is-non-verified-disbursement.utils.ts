import { ITransaction } from "../../queries/search-transactions.decoder";
import { ApplicationError } from "../application-error";
import { TaskEither } from "fp-ts/TaskEither";
import { taskEither } from "fp-ts";
import { TransactionType } from "../enums/transaction-type.enum";

export const isNonRuleVerifiedDisb = (
  t: ITransaction
): TaskEither<ApplicationError, ITransaction> => {
  if (
    t.transactionType === TransactionType.Disbursement &&
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

export const isNonBankVerifiedDisb = (
  t: ITransaction
): TaskEither<ApplicationError, ITransaction> => {
  if (
    t.transactionType === TransactionType.Disbursement &&
    !t.bankVerified &&
    t.ruleVerified
  ) {
    return taskEither.right(t);
  } else {
    return taskEither.left(
      new ApplicationError("Transaction is not an unverified disbursement.", {})
    );
  }
};

const isBankVerifiedAndRuleUnverified = (
  bankVerified: boolean,
  ruleVerified: boolean
) => bankVerified && !ruleVerified;

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
