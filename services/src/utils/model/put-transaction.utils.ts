import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../queries/search-transactions.decoder";
import { pipe } from "fp-ts/function";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../application-error";
import { taskEither as te } from "fp-ts";

export const putTransaction =
  (txnTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (txn: ITransaction) => {
    console.log("Transaction writing to ddb");
    const marshalledTxn = DynamoDB.Converter.marshall(txn);
    await dynamoDB
      .putItem({
        TableName: txnTableName,
        Item: marshalledTxn,
      })
      .promise();
    return txn;
  };

export const putTransactionAndDecode =
  (txnTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (txn: ITransaction): TaskEither<ApplicationError, ITransaction> =>
    pipe(
      te.tryCatch(
        () => putTransaction(txnTableName)(dynamoDB)(txn),
        (e) => new ApplicationError("Put transaction failed", {})
      )
    );
