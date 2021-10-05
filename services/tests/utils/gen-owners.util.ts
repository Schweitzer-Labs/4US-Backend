import {
  IOwner,
  ITransaction,
} from "../../src/queries/search-transactions.decoder";
import * as faker from "faker";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import { Direction } from "../../src/utils/enums/direction.enum";
import { PaymentMethod } from "../../src/utils/enums/payment-method.enum";
import { TransactionType } from "../../src/utils/enums/transaction-type.enum";
import { now } from "../../src/utils/time.utils";
import { EntityType } from "../../src/utils/enums/entity-type.enum";
import { genCommittee } from "./gen-committee.util";

const sum = (nums: number[]) => nums.reduce((acc, val) => acc + val, 0);
const roundToCents = (num: number) => Math.round(num * 100) / 100;

export const genOwner = (percentOwnership: string): IOwner => ({
  firstName: faker.name.firstName(),
  lastName: faker.name.lastName(),
  addressLine1: faker.address.streetAddress(),
  city: faker.address.city(),
  state: faker.address.stateAbbr().toLowerCase(),
  postalCode: faker.address.zipCode(),
  percentOwnership,
});

export const genTxnWithOwners = (owners: IOwner[]): ITransaction => ({
  id: genTxnId(),
  committeeId: genTxnId(),
  direction: Direction.In,
  paymentMethod: PaymentMethod.Check,
  transactionType: TransactionType.Contribution,
  bankVerified: false,
  ruleVerified: false,
  initiatedTimestamp: now(),
  paymentDate: now(),
  source: "abc",
  amount: faker.datatype.number(4950) + 50,
  entityType: EntityType.Llc,
  owners,
});

const genPercents = () => {
  let col = [];
  let current = 0;
  let currSum = 0;

  while (sum(col) <= 100) {
    currSum = sum(col);
    current = faker.datatype.number(100);

    if (currSum + current <= 100) {
      col.push(current);
    } else {
      break;
    }
  }
  const resSum = sum(col);
  if (resSum === 100) {
    return col;
  } else {
    const remaining = 100 - resSum;
    col.push(remaining);
    return col;
  }
};

export const genOwners = () => {
  return genPercents().map((num) => genOwner(num + ""));
};
