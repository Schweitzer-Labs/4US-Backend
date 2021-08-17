import { DynamoDB } from "aws-sdk";
import { deleteTxn } from "../../utils/model/delete-txn.utils";
import { putTransaction } from "../../utils/model/put-transaction.utils";

const withinADayOf = (d1) => (d2) => Math.abs(d1 - d2) < 1000 * 60 * 60 * 24;
export const runRec =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (payoutGroups) =>
  async (bankPayouts) => {
    let reconCount = 0;
    for (const txn of bankPayouts) {
      const matches = payoutGroups.filter((payout) => {
        return (
          txn.amount === payout.amount &&
          withinADayOf(txn.paymentDate)(payout.payoutDate)
        );
      });
      if (matches.length === 1) {
        const match = matches[0];
        console.log("match found");
        console.log("bank txn", txn);
        await deleteTxn(txnsTableName)(dynamoDB)(txn);
        for (const pTxn of match.txns) {
          console.log("setting txns to verified");
          await putTransaction(txnsTableName)(dynamoDB)({
            ...pTxn,
            finicityTransactionData: txn.finicityTransactionData,
            finicityTransactionId: txn.finicityTransactionId,
            finicityCategory: txn.finicityCategory,
            finicityBestRepresentation: txn.finicityBestRepresentation,
            finicityPostedDate: txn.finicityPostedDate,
            finicityTransactionDate: txn.finicityTransactionDate,
            finicityNormalizedPayeeName: txn.finicityNormalizedPayeeName,
            finicityDescription: txn.finicityDescription,
            bankVerified: true,
          });
          reconCount++;
        }
      }
    }
    console.log("rec count", reconCount);
  };
