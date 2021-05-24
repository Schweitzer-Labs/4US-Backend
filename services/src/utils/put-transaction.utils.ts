import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../queries/search-transactions.decoder";

export const putTransaction =
  (txnTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (txn: ITransaction) => {
    const marshalledCommittee = DynamoDB.Converter.marshall(txn);
    return await dynamoDB
      .putItem({
        TableName: txnTableName,
        Item: marshalledCommittee,
      })
      .promise();
  };
