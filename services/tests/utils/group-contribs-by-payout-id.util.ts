import {
  ISyncContribResult,
  Result,
} from "../../src/pipes/external-contribs/external-txns-to-ddb.pipe";
import { ITransaction } from "../../src/model/transaction.type";

interface IPayoutGroups {
  [key: string]: ITransaction[];
}

export const groupContribByPayoutId = (
  syncRes: ISyncContribResult[]
): IPayoutGroups =>
  syncRes.reduce((acc, res) => {
    if (res.result === Result.Created) {
      const newPayoutId = res.contribTransaction.externalTransactionPayoutId;
      acc[newPayoutId] = [
        ...(acc[newPayoutId] ? acc[newPayoutId] : []),
        res.contribTransaction,
      ];
      return acc;
    } else {
      return acc;
    }
  }, {});
