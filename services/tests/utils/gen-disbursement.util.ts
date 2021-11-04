import { genTransaction } from "./gen-transaction.util";
import { Direction } from "../../src/utils/enums/direction.enum";
import { TransactionType } from "../../src/utils/enums/transaction-type.enum";
import { ITransaction } from "../../src/types/transaction.type";

interface GenDisbursementConfig {
  committeeId: string;
  bankVerified: boolean;
  ruleVerified: boolean;
}

export const genDisbursement = (
  config: GenDisbursementConfig
): ITransaction => ({
  ...genTransaction({
    direction: Direction.Out,
    ...config,
  }),
  transactionType: TransactionType.Disbursement,
});
