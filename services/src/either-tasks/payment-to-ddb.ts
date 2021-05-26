import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { StatusCodes } from "http-status-codes";
import { v4 as uuidv4 } from "uuid";
import { Payment } from "./contribution-to-payment";
import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../queries/search-transactions.decoder";
import { now } from "../utils/time.utils";
import { Source } from "../utils/enums/source.enum";
import { genTxnId } from "../utils/gen-txn-id.utils";
import { Direction } from "../utils/enums/direction.enum";
import { TransactionType } from "../utils/enums/transaction-type.enum";

const savePayment =
  (transactionsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (payment: Payment): Promise<any> => {
    const transaction: ITransaction = {
      ...payment,
      id: genTxnId(),
      committeeId: payment.committee,
      source: Source.DONATE_FORM,
      direction: Direction.IN,
      bankVerified: false,
      ruleVerified: false,
      initiatedTimestamp: now(),
      transactionType: TransactionType.CONTRIBUTION,
    };

    console.log(
      `Writing contribution to ${transactionsTableName}`,
      transaction
    );

    const payload = DynamoDB.Converter.marshall(transaction);
    return await dynamoDB
      .putItem({
        TableName: transactionsTableName,
        Item: payload,
      })
      .promise();
  };

export const paymentToDDB =
  (transactionsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (payment: Payment): TaskEither<ApplicationError, any> =>
    tryCatch<ApplicationError, any>(
      () => savePayment(transactionsTableName)(dynamoDB)(payment),
      (error) =>
        new ApplicationError(
          "DDB Write failed",
          error,
          StatusCodes.UNAUTHORIZED
        )
    );
