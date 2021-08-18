import { ITransaction } from "../../src/queries/search-transactions.decoder";
import { genTransaction } from "./gen-transaction.util";
import { PaymentMethod } from "../../src/utils/enums/payment-method.enum";
import * as faker from "faker";
import { TransactionType } from "../../src/utils/enums/transaction-type.enum";
import { Direction } from "../../src/utils/enums/direction.enum";
import {
  FinicityTransaction,
  IFinicityTransaction,
} from "../../src/clients/finicity/finicity.decoders";
import * as t from "io-ts";

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
}: GenFinicityTxn): ITransaction => {
  const fData: IFinicityTransaction = {
    id: faker.datatype.number(5000000),
    amount: faker.datatype.number(5000000),
    accountId: faker.datatype.number(5000000),
    status: "complete",
    description: faker.commerce.productDescription(),
    postedDate: paymentDate,
    transactionDate: paymentDate,
    createdDate: paymentDate,
    customerId: 123,
    type: "credit",
    categorization: {
      normalizedPayeeName: faker.commerce.product(),
      category: faker.commerce.department(),
      bestRepresentation: faker.commerce.productDescription(),
      country: "US",
    },
  };

  return {
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
    finicityTransactionData: fData,
    transactionType: dirToTxnType(direction),
    finicityCategory: fData.categorization.category,
    finicityBestRepresentation: fData.categorization.bestRepresentation,
    finicityPostedDate: fData.postedDate,
    finicityTransactionDate: fData.transactionDate,
    finicityNormalizedPayeeName: fData.categorization.normalizedPayeeName,
    finicityDescription: fData.description,
    finicityTransactionId: fData.id,
    finicityPaymentMethod: fData.type,
  };
};

const dirToTxnType = (dir: Direction) =>
  dir === Direction.In
    ? TransactionType.Contribution
    : TransactionType.Disbursement;
