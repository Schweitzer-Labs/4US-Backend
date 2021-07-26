import { ITransaction } from "../../src/queries/search-transactions.decoder";
import { genTransaction } from "./gen-transaction.util";
import { PaymentMethod } from "../../src/utils/enums/payment-method.enum";
import * as faker from "faker";
import { TransactionType } from "../../src/utils/enums/transaction-type.enum";
import { Direction } from "../../src/utils/enums/direction.enum";

interface GenFinicityTxn {
  committeeId: string;
  paymentMethod: PaymentMethod;
  amount?: number;
  paymentDate: number;
  direction: Direction;
}

export const genFinicityTxn = ({
  amount,
  committeeId,
  paymentDate,
  paymentMethod,
  direction,
}: GenFinicityTxn): ITransaction => ({
  ...genTransaction({
    bankVerified: true,
    ruleVerified: false,
    committeeId,
    amount,
    paymentDate,
    paymentMethod,
    initiatedTimestamp: paymentDate,
    direction,
  }),
  transactionType: dirToTxnType(direction),
  finicityCategory: faker.commerce.department(),
  finicityBestRepresentation: faker.commerce.productDescription(),
  finicityPostedDate: paymentDate,
  finicityTransactionDate: paymentDate,
  finicityNormalizedPayeeName: faker.commerce.product(),
  finicityDescription: faker.commerce.productName(),
});

const dirToTxnType = (dir: Direction) =>
  dir === Direction.In
    ? TransactionType.Contribution
    : TransactionType.Disbursement;
