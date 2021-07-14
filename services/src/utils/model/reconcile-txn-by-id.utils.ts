import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../queries/search-transactions.decoder";
import { UpdateItemInput } from "aws-sdk/clients/dynamodb";

const updateExpressionList = [
  "#bankVerified = :bankVerified",
  "#finicityTransactionId = :finicityTransactionId",
  "#finicityNormalizedPayeeName = :finicityNormalizedPayeeName",
  "#finicityCategory = :finicityCategory",
  "#finicityBestRepresentation = :finicityBestRepresentation",
  "#finicityDescription = :finicityDescription",
  "#finicityPostedDate = :finicityPostedDate",
  "#finicityTransactionDate = :finicityTransactionDate",
];

export const reconcileTxnById =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (txn: ITransaction) =>
  async (txnId: string): Promise<any> => {
    console.log("update function called");
    const params: UpdateItemInput = {
      TableName: txnsTableName,
      Key: {
        id: {
          S: txnId,
        },
        committeeId: {
          S: txn.committeeId,
        },
      },
      UpdateExpression: `set ${updateExpressionList.join(", ")}`,
      ExpressionAttributeNames: {
        "#bankVerified": "bankVerified",
        "#finicityTransactionId": "finicityTransactionId",
        "#finicityNormalizedPayeeName": "finicityNormalizedPayeeName",
        "#finicityCategory": "finicityCategory",
        "#finicityBestRepresentation": "finicityBestRepresentation",
        "#finicityDescription": "finicityDescription",
        "#finicityPostedDate": "finicityPostedDate",
        "#finicityTransactionDate": "finicityTransactionDate",
      },
      ExpressionAttributeValues: {
        ":bankVerified": {
          BOOL: true,
        },
        ":finicityTransactionId": {
          N: "" + txn.finicityTransactionId,
        },
        ":finicityNormalizedPayeeName": {
          S: txn.finicityNormalizedPayeeName,
        },
        ":finicityCategory": {
          S: txn.finicityCategory,
        },
        ":finicityBestRepresentation": {
          S: txn.finicityBestRepresentation,
        },
        ":finicityDescription": {
          S: txn.finicityDescription,
        },
        ":finicityPostedDate": {
          N: "" + txn.finicityPostedDate,
        },
        ":finicityTransactionDate": {
          N: "" + txn.finicityTransactionDate,
        },
        ":finicityTransactionData": {
          M: DynamoDB.Converter.marshall(txn.finicityTransactionData),
        },
      },
    };
    console.log("params", params);
    const res = await dynamoDB.updateItem(params).promise();
    console.log("Transaction inserted with stripe payout ID");
    return res;
  };

export const reconcileTxnsByIds =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (txn: ITransaction) =>
  async (txnIds: string[]): Promise<any> => {
    for (const txnId of txnIds) {
      console.log(
        "Attempting to update txn with stripe payout ID",
        JSON.stringify(txn)
      );
      const res = await reconcileTxnById(txnsTableName)(dynamoDB)(txn)(txnId);
      console.log("Update result", res);
    }
    return true;
  };
