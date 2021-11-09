import * as t from "io-ts";
import { State } from "../utils/enums/state.enum";
import { fromEnum } from "../utils/from-enum.utils";
import { EmploymentStatus } from "../utils/enums/employment-status";

const ExternalTxnReq = t.type({
  id: t.string,
  recipientId: t.string,
  source: t.string,
  paymentDate: t.number,
  amount: t.number,
  firstName: t.string,
  lastName: t.string,
  addressLine1: t.string,
  city: t.string,
  state: fromEnum<State>("State", State),
  country: t.string,
  postalCode: t.string,
});

const ExternalTxnOpt = t.partial({
  emailAddress: t.string,
  employer: t.string,
  employmentStatus: fromEnum<EmploymentStatus>(
    "EmploymentStatus",
    EmploymentStatus
  ),
  recipientGovId: t.string,
  occupation: t.string,
  refCode: t.string,
  addressLine2: t.string,
  phoneNumber: t.string,
  middleName: t.string,
  reportType: t.string,
  metadata: t.unknown,
});

export const ExternalTxn = t.intersection([ExternalTxnReq, ExternalTxnOpt]);

export type IExternalTxn = t.TypeOf<typeof ExternalTxn>;

export const ExternalData = t.type({
  transactions: t.array(ExternalTxn),
});

export type IExternalData = t.TypeOf<typeof ExternalData>;
