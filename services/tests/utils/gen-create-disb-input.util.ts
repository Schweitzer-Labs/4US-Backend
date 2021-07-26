import { CreateDisbursementInput } from "../../src/input-types/create-disbursement.input-type";
import * as faker from "faker";
import { PaymentMethod } from "../../src/utils/enums/payment-method.enum";
import {
  PurposeCode,
  purposeCodes,
} from "../../src/utils/enums/purpose-code.enum";
import { now } from "../../src/utils/time.utils";

interface GenCreateDisbConfig {
  committeeId: string;
  paymentMethod?: PaymentMethod;
  amount?: number;
  entityName?: string;
  addressLine1?: string;
  city?: string;
  state?: string;
  postalCode?: string;
  isSubcontracted?: boolean;
  isPartialPayment?: boolean;
  isExistingLiability?: boolean;
  purposeCode?: PurposeCode;
  paymentDate?: number;
  checkNumber?: string;
  addressLine2?: string;
}

export const genCreateDisbInput = ({
  committeeId,
  amount = faker.datatype.number({
    min: 1000,
    max: 5000,
  }),
  entityName = faker.company.companyName(),
  paymentMethod = PaymentMethod.Debit,
  addressLine1 = faker.address.streetAddress(),
  city = faker.address.city(),
  state = faker.address.stateAbbr().toLowerCase(),
  postalCode = faker.address.zipCode(),
  paymentDate = now(),
}: GenCreateDisbConfig): CreateDisbursementInput => ({
  committeeId,
  amount,
  paymentMethod,
  entityName,
  addressLine1,
  city,
  state,
  postalCode,
  isSubcontracted: false,
  isPartialPayment: false,
  isExistingLiability: false,
  purposeCode: faker.random.arrayElement(purposeCodes),
  paymentDate: paymentDate,
});
