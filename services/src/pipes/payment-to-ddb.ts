import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { StatusCodes } from "http-status-codes";
import { Payment } from "./contribution-to-payment";
import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../model/transaction.type";
import { now } from "../utils/time.utils";
import { Source } from "../utils/enums/source.enum";
import { genTxnId } from "../utils/gen-txn-id.utils";
import { Direction } from "../utils/enums/direction.enum";
import { TransactionType } from "../utils/enums/transaction-type.enum";
import { putTransaction } from "../utils/model/transaction/put-transaction.utils";

const savePayment =
  (transactionsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (payment: Payment): Promise<any> => {
    console.log("Payment saving to transactions able");
    const transaction: ITransaction = {
      ...payment,
      paymentDate: now(),
      id: genTxnId(),
      source: Source.DONATE_FORM,
      direction: Direction.In,
      bankVerified: false,
      initiatedTimestamp: now(),
      transactionType: TransactionType.Contribution,
    };

    console.log(
      `Writing contribution to ${transactionsTableName}`,
      transaction
    );
    return await putTransaction(transactionsTableName)(dynamoDB)(transaction);
  };

export const paymentToDDB =
  (transactionsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (payment: Payment): TaskEither<ApplicationError, ITransaction> =>
    tryCatch<ApplicationError, any>(
      () => savePayment(transactionsTableName)(dynamoDB)(payment),
      (error) =>
        new ApplicationError(
          "Payment successful, DDB write failed",
          error,
          StatusCodes.UNAUTHORIZED
        )
    );
