import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../queries/search-transactions.decoder";

export const putTransaction =
  (txnTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (txn: ITransaction) => {
    const marshalledTxn = DynamoDB.Converter.marshall(txn);
    await dynamoDB
      .putItem({
        TableName: txnTableName,
        Item: marshalledTxn,
      })
      .promise();
    return txn;
  };
