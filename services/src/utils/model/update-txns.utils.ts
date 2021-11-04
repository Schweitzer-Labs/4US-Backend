import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../model/transaction.type";
import { UpdateItemInput } from "aws-sdk/clients/dynamodb";

export const updateTxnWithStripePayoutId =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (txn: ITransaction): Promise<any> => {
    console.log("update function called");
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
      UpdateExpression: "set #payoutId = :payoutId, #payoutDate = :payoutDate",
      ExpressionAttributeNames: {
        "#payoutId": "stripePayoutId",
        "#payoutDate": "stripeAutomaticPayoutEffectiveAtUtc",
      },
      ExpressionAttributeValues: {
        ":payoutId": {
          S: txn.stripePayoutId,
        },
        ":payoutDate": {
          N: "" + txn.stripeAutomaticPayoutEffectiveAtUtc,
        },
      },
    };
    console.log("params", params);
    const res = await dynamoDB.updateItem(params).promise();
    console.log("Transaction inserted with stripe payout ID");
    return res;
  };

export const update_txns_with_stripe_payout_id =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (txns: ITransaction[]): Promise<any> => {
    for (const txn of txns) {
      console.log(
        "Attempting to update txn with stripe payout ID",
        JSON.stringify(txn)
      );
      const res = await updateTxnWithStripePayoutId(txnsTableName)(dynamoDB)(
        txn
      );
      console.log("Update result", res);
    }
    return true;
  };
