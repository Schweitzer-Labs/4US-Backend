import { ITransaction } from "../../src/queries/search-transactions.decoder";
import { genTransaction } from "./gen-transaction.util";
import { Direction } from "../../src/utils/enums/direction.enum";
import { PaymentMethod } from "../../src/utils/enums/payment-method.enum";
import { EntityType } from "../../src/utils/enums/entity-type.enum";
import * as faker from "faker";
import { TransactionType } from "../../src/utils/enums/transaction-type.enum";
import { CreateContributionInput } from "../../src/input-types/create-contribution.input-type";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";

export const genContributionRecord = (
  committeeId: string,
  donorId?: string,
  entityType?: EntityType
): ITransaction => {
  return {
    ...genTransaction({
      committeeId,
      direction: Direction.IN,
      paymentMethod: PaymentMethod.Credit,
    }),
    transactionType: TransactionType.CONTRIBUTION,
    committeeId,
    donorId: donorId || genTxnId(),
    entityType: entityType || EntityType.IND,
    firstName: faker.name.firstName(),
    lastName: faker.name.lastName(),
    addressLine1: faker.address.streetAddress(),
    city: faker.address.city(),
    state: faker.address.state(),
    postalCode: faker.address.zipCode(),
    ...(![EntityType.IND, EntityType.FAM].includes(entityType)
      ? { entityName: faker.company.companyName() }
      : {}),
  };
};

export const genCreateContributionInput = (
  committeeId: string,
  amount: number,
  entityType?: EntityType
): CreateContributionInput => {
  return {
    amount,
    committeeId,
    entityType,
    paymentMethod: PaymentMethod.Credit,
    firstName: faker.name.firstName(),
    lastName: faker.name.lastName(),
    addressLine1: faker.address.streetAddress(),
    city: faker.address.city(),
    state: faker.address.state(),
    postalCode: faker.address.zipCode(),
    ...(![EntityType.IND, EntityType.FAM].includes(entityType)
      ? { entityName: faker.company.companyName() }
      : {}),
  };
};