import { ITransaction } from "../../model/transaction.type";
import { PaymentMethod } from "../enums/payment-method.enum";
import { ApplicationError } from "../application-error";
import { taskEither as te } from "fp-ts";
import { TaskEither } from "fp-ts/TaskEither";
import { pipe } from "fp-ts/function";

export const validateDisbursement = (
  txn: ITransaction
): TaskEither<ApplicationError, ITransaction> =>
  pipe(
    txnHasAPurposeCode(txn),
    te.chain(txnHasAPaymentDate),
    te.chain(txnHasCheckNumberIfCheck),
    te.chain(txnHasAnAddress),
    te.chain(txnHasAnsweredComplianceQuestions)
  );

export const isBool = (a: unknown): boolean => typeof a === "boolean";

const txnHasAnsweredComplianceQuestions = (
  txn: ITransaction
): TaskEither<ApplicationError, ITransaction> => {
  if (
    [txn.isExistingLiability, txn.isSubcontracted, txn.isPartialPayment].every(
      isBool
    )
  ) {
    return te.right(txn);
  }
  return te.left(
    new ApplicationError(
      "Disbursement must have compliance questions answered",
      {}
    )
  );
};

const txnHasAPurposeCode = (
  txn: ITransaction
): TaskEither<ApplicationError, ITransaction> => {
  if (txn.purposeCode) {
    return te.right(txn);
  }
  return te.left(
    new ApplicationError("Disbursement must have a purpose code", {})
  );
};

const txnHasAPaymentDate = (
  txn: ITransaction
): TaskEither<ApplicationError, ITransaction> => {
  if (txn.paymentDate) {
    return te.right(txn);
  }
  return te.left(
    new ApplicationError("Transaction must have a payment date", {})
  );
};

const txnHasCheckNumberIfCheck = (
  txn: ITransaction
): TaskEither<ApplicationError, ITransaction> => {
  if (txn.paymentMethod === PaymentMethod.Check && !txn.checkNumber) {
    return te.left(
      new ApplicationError(
        "Check number must be provided for transactions made with checks",
        {}
      )
    );
  }
  return te.right(txn);
};

const txnHasAnAddress = (
  txn: ITransaction
): TaskEither<ApplicationError, ITransaction> => {
  if (txn.addressLine1 && txn.city && txn.state && txn.postalCode) {
    return te.right(txn);
  }
  return te.left(new ApplicationError("Transaction must contain address", {}));
};
