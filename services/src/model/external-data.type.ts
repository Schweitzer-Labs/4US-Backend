import * as t from "io-ts";
import { State } from "../utils/enums/state.enum";
import { fromEnum } from "../utils/from-enum.utils";
import { EmploymentStatus } from "../utils/enums/employment-status";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { ICommittee } from "./committee.type";
import { ITransaction } from "./transaction.type";

const ExternalContribReq = t.type({
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

const ExternalContribOpt = t.partial({
  payoutDate: t.string,
  payoutId: t.string,
  fee: t.number,
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

export const ExternalContrib = t.intersection([
  ExternalContribReq,
  ExternalContribOpt,
]);

export type IExternalContrib = t.TypeOf<typeof ExternalContrib>;

export const ExternalData = t.type({
  contributions: t.array(ExternalContrib),
});

export type IExternalData = t.TypeOf<typeof ExternalData>;

export type ContributionMapper = <a>(a) => IExternalContrib;

export type ContributionDoesNotExist = <a>(
  comTable
) => () => (ddb) => (a) => ITransaction;

export type CommitteeGetter = <args>(
  string
) => (string) => (args) => TaskEither<ApplicationError, ICommittee>;
