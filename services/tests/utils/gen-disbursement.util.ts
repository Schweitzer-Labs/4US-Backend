import { genTransaction } from "./gen-transaction.util";
import { Direction } from "../../src/utils/enums/direction.enum";
import { TransactionType } from "../../src/utils/enums/transaction-type.enum";
import { ITransaction } from "../../src/queries/search-transactions.decoder";

interface GenDisbursementConfig {
  committeeId: string;
  bankVerified: boolean;
  ruleVerified: boolean;
}

export const genDisbursement = (
  config: GenDisbursementConfig
): ITransaction => ({
  ...genTransaction({
    direction: Direction.OUT,
    ...config,
  }),
  transactionType: TransactionType.DISBURSEMENT,
});
