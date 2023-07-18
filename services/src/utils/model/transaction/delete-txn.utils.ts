import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../../model/transaction.type";
import { ApplicationError } from "../../application-error";
import { tryCatch } from "fp-ts/TaskEither";

export const deleteTxn =
  (transactionTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (txn: ITransaction) => {
    console.log("delete txn is called with ", txn.id);
    await dynamoDB
      .deleteItem({
        TableName: transactionTableName,
        Key: {
          id: {
            S: txn.id,
          },
          committeeId: {
            S: txn.committeeId,
          },
        },
      })
      .promise();
    console.log("Transaction deleted. ID: " + txn.id);
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
