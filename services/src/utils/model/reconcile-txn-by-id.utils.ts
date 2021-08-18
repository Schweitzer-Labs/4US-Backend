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
  "#finicityPaymentMethod = :finicityPaymentMethod",
  "#finicityTransactionDate = :finicityTransactionDate",
  "#finicityTransactionData = :finicityTransactionData",
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
        "#finicityPaymentMethod": "finicityPaymentMethod",
        "#finicityTransactionDate": "finicityTransactionDate",
        "#finicityTransactionData": "finicityTransactionData",
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
        ":finicityPaymentMethod": {
          S: txn.finicityPaymentMethod,
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
      console.log("Reconciling txns", JSON.stringify(txn));
      const res = await reconcileTxnById(txnsTableName)(dynamoDB)(txn)(txnId);

      console.log("Update result", res);
    }
    return true;
  };
