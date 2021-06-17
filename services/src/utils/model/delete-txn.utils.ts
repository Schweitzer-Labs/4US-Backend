import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../queries/search-transactions.decoder";
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
