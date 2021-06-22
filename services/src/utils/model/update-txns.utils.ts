import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../queries/search-transactions.decoder";
import { pipe } from "fp-ts/function";

export const updateTxns =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (txns: ITransaction) => {
    const res = dynamoDB.updateItem({
      TableName: txnsTableName,
    });
  };
