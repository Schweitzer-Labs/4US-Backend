import { ITransaction } from "../../src/queries/search-transactions.decoder";
import { genTransaction } from "./gen-transaction.util";
import { Direction } from "../../src/utils/enums/direction.enum";
import { PaymentMethod } from "../../src/utils/enums/payment-method.enum";

export const genContributionRecord = (
  committeeId: string,
  donorId: string
): ITransaction => {
  return {
    ...genTransaction({
      committeeId,
      direction: Direction.IN,
      paymentMethod: PaymentMethod.Credit,
    }),
    committeeId,
    donorId,
  };
};
