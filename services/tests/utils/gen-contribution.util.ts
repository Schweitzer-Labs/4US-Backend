import { ITransaction } from "../../src/queries/search-transactions.decoder";
import { genTransaction } from "./gen-transaction.util";
import { Direction } from "../../src/utils/enums/direction.enum";
import { PaymentMethod } from "../../src/utils/enums/payment-method.enum";
import { EntityType } from "../../src/utils/enums/entity-type.enum";
import * as faker from "faker";
import { TransactionType } from "../../src/utils/enums/transaction-type.enum";
import { CreateContributionInput } from "../../src/input-types/create-contribution.input-type";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import { now } from "../../src/utils/time.utils";
import { enumToValues } from "../../src/utils/enums/poly.util";
import { State } from "../../src/utils/enums/state.enum";

export const genContributionRecord = (
  committeeId: string,
  donorId?: string,
  entityType?: EntityType
): ITransaction => {
  return {
    ...genTransaction({
      committeeId,
      direction: Direction.In,
      paymentMethod: PaymentMethod.Credit,
      ruleVerified: true,
      bankVerified: false,
      paymentDate: now(),
    }),
    transactionType: TransactionType.Contribution,
    committeeId,
    donorId: donorId || genTxnId(),
    entityType: entityType || EntityType.Ind,
    firstName: faker.name.firstName(),
    lastName: faker.name.lastName(),
    addressLine1: faker.address.streetAddress(),
    city: faker.address.city(),
    state: faker.random.arrayElement(enumToValues(State)),
    postalCode: faker.address.zipCode(),
    ...(![EntityType.Ind, EntityType.Fam].includes(entityType)
      ? { entityName: faker.company.companyName() }
      : {}),
  };
};
