import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../queries/search-transactions.decoder";
import { UpdateItemInput } from "aws-sdk/clients/dynamodb";

export const updateTxnWithStripePayoutId =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (txn: ITransaction): Promise<any> => {
    const params: UpdateItemInput = {
      TableName: txnsTableName,
      Key: {
        id: {
          S: txn.id,
        },
        committeeId: {
          S: txn.committeeId,
        },
      },
      UpdateExpression: "set stripePayoutId = :stripePayoutId",
      ExpressionAttributeValues: {
        ":stripePayoutId": {
          S: txn.stripePayoutId,
        },
      },
    };
    const res = await dynamoDB.updateItem(params).promise();
    console.log("Transaction inserted with stripe payout ID");
    return res;
  };
