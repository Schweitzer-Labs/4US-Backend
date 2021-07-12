import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../queries/search-transactions.decoder";
import { ApplicationError } from "../application-error";
import { tryCatch } from "fp-ts/TaskEither";

export const deleteTxn =
  (transactionTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (txn: ITransaction) => {
    await dynamoDB
      .deleteItem({
        TableName: transactionTableName,
        Key: {
          id: {
            S: txn.id,
          },
        },
      })
      .promise();
    return txn;
  };

export const deleteTxnPipe =
  (transactionTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (txn: ITransaction) =>
    tryCatch(
      () => deleteTxn(transactionTableName)(dynamoDB)(txn),
      (e) => new ApplicationError("Txn delete failed", e)
    );
