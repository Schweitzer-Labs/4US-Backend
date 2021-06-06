import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import { Direction, directions } from "../../src/utils/enums/direction.enum";
import * as faker from "faker";
import { ITransaction } from "../../src/queries/search-transactions.decoder";
import { now } from "../../src/utils/time.utils";
import { Source, sources } from "../../src/utils/enums/source.enum";
import { PaymentMethod } from "../../src/utils/enums/payment-method.enum";

interface IGenTransactionConfig {
  id?: string;
  committeeId?: string;
  direction?: Direction;
  bankVerified: boolean;
  ruleVerified: boolean;
  paymentMethod?: PaymentMethod;
  amount?: number;
  initiatedTimestamp?: number;
  source?: Source;
}

export const genTransaction = ({
  id,
  committeeId,
  direction,
  bankVerified,
  ruleVerified,
  paymentMethod,
  amount,
  initiatedTimestamp,
  source,
}: IGenTransactionConfig): ITransaction => ({
  // Required fields
  id: id || genTxnId(),
  committeeId: committeeId || genTxnId(),
  direction: direction || faker.random.arrayElement(directions),
  amount:
    amount ||
    faker.datatype.number({
      min: 1000,
      max: 5000,
    }),
  initiatedTimestamp: initiatedTimestamp || now(),
  bankVerified,
  ruleVerified,
  paymentMethod: paymentMethod || PaymentMethod.Ach,
  source: source || Source.DASHBOARD,
});
