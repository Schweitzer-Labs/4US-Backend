import { expect } from "chai";
import {
  IOwner,
  ITransaction,
} from "../../src/queries/search-transactions.decoder";
import * as faker from "faker";
import { Direction } from "../../src/utils/enums/direction.enum";
import { PaymentMethod } from "../../src/utils/enums/payment-method.enum";
import { now } from "../../src/utils/time.utils";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import { genCommittee } from "../utils/gen-committee.util";
import {
  centsToDollars,
  toAttributedContribs,
} from "../../src/pipes/generate-disclosure.pipe";
import { EntityType } from "../../src/utils/enums/entity-type.enum";
import { TransactionType } from "../../src/utils/enums/transaction-type.enum";

const genOwner = (percentOwnership: string): IOwner => ({
  firstName: faker.name.firstName(),
  lastName: faker.name.lastName(),
  addressLine1: faker.address.streetAddress(),
  city: faker.address.city(),
  state: faker.address.stateAbbr().toLowerCase(),
  postalCode: faker.address.zipCode(),
  percentOwnership,
});

const genTxnWithOwners = (owners: IOwner[]): ITransaction => ({
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

const committee = genCommittee({ scope: "local" });

const genOwners = () => {
  return genPercents().map((num) => genOwner(num + ""));
};

const sum = (nums: number[]) => nums.reduce((acc, val) => acc + val, 0);
const roundToCents = (num: number) => Math.round(num * 100) / 100;

describe("Ownership attribution", function () {
  it("Properly attributes ownership", () => {
    for (let i = 0; i < 100; i++) {
      console.log("\n", "------------------------------", "\n");
      const owners = genOwners();
      const txn = genTxnWithOwners(owners);

      const res = toAttributedContribs(committee)(txn);

      console.log(
        "owner percentages",
        owners.map((val) => val.percentOwnership)
      );

      console.log("txn total", txn.amount);

      const totalAmount = res.reduce((acc, val) => {
        console.log("before parse", val["ORG_AMT"]);
        const amount = Math.round(parseFloat(val["ORG_AMT"]) * 100);
        console.log("parsed amount", amount);

        return amount + acc;
      }, 0);

      console.log("number of owners tested", i);
      expect(totalAmount).to.equal(txn.amount);
    }
  });
});
