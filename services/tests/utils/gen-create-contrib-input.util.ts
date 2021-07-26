import { EntityType } from "../../src/utils/enums/entity-type.enum";
import { CreateContributionInput } from "../../src/input-types/create-contribution.input-type";
import * as faker from "faker";
import { PaymentMethod } from "../../src/utils/enums/payment-method.enum";
import { now } from "../../src/utils/time.utils";

export const genCreateContribInput = (
  committeeId: string,
  amount?: number,
  entityType?: EntityType,
  paymentDate?: number
): CreateContributionInput => {
  return {
    amount:
      amount ||
      faker.datatype.number({
        min: 1000,
        max: 5000,
      }),
    committeeId,
    entityType: entityType || EntityType.Ind,
    paymentMethod: PaymentMethod.Credit,
    firstName: faker.name.firstName(),
    lastName: faker.name.lastName(),
    addressLine1: faker.address.streetAddress(),
    city: faker.address.city(),
    state: faker.address.state(),
    postalCode: faker.address.zipCode(),
    cardNumber: "4242 4242 4242 4242",
    cardExpirationYear: 2026,
    cardExpirationMonth: 12,
    cardCVC: "123",
    paymentDate: paymentDate || now(),
    ...(![EntityType.Ind, EntityType.Fam].includes(entityType)
      ? { entityName: faker.company.companyName() }
      : {}),
  };
};
