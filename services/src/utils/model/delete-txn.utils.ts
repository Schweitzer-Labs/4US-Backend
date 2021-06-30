import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../queries/search-transactions.decoder";
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
    console.log("success deleting");
    return txn;
  };
