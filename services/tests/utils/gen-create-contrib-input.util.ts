import { EntityType } from "../../src/utils/enums/entity-type.enum";
import { CreateContributionInput } from "../../src/graphql/input-types/create-contribution.input-type";
import * as faker from "faker";
import { PaymentMethod } from "../../src/utils/enums/payment-method.enum";
import { now } from "../../src/utils/time.utils";
import { EmploymentStatus } from "../../src/utils/enums/employment-status";
import { enumToKeys } from "../../src/utils/enums/poly.util";
import { State } from "../../src/utils/enums/state.enum";

interface GenCreateContribInputConfig {
  committeeId: string;
  amount?: number;
  entityType?: EntityType;
  paymentDate?: number;
  paymentMethod?: PaymentMethod;
  checkNumber?: string;
}

export const genCreateContribInput = ({
  committeeId,
  amount,
  entityType,
  paymentDate,
  paymentMethod,
  checkNumber,
}: GenCreateContribInputConfig): CreateContributionInput => {
  const stateStr: any = faker.random.arrayElement(enumToKeys(State));
  return {
    amount:
      amount ||
      faker.datatype.number({
        min: 1000,
        max: 5000,
      }),
    committeeId,
    checkNumber,
    entityType: entityType || EntityType.Ind,
    paymentMethod: paymentMethod || PaymentMethod.Credit,
    firstName: faker.name.firstName(),
    lastName: faker.name.lastName(),
    addressLine1: faker.address.streetAddress(),
    city: faker.address.city(),
    state: stateStr,
    postalCode: faker.address.zipCode(),
    cardNumber: "4242 4242 4242 4242",
    cardExpirationYear: 2026,
    cardExpirationMonth: 12,
    cardCVC: "123",
    processPayment: true,
    employmentStatus: EmploymentStatus.Unemployed,
    paymentDate: paymentDate || now(),
    ...(![EntityType.Ind, EntityType.Fam].includes(entityType)
      ? { entityName: faker.company.companyName() }
      : {}),
  };
};
