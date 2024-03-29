import * as t from "io-ts";
import { State } from "../utils/enums/state.enum";
import { fromEnum } from "../utils/from-enum.utils";
import { EmploymentStatus } from "../utils/enums/employment-status";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { ICommittee } from "./committee.type";
import { DynamoDB } from "aws-sdk";
import { EntityType } from "../utils/enums/entity-type.enum";
import { Stripe } from "stripe";
import { ILexisNexisConfig } from "../clients/lexis-nexis/lexis-nexis.client";
import { PaymentMethod } from "../utils/enums/payment-method.enum";
import { Source } from "../utils/enums/source.enum";

const ExternalContribReq = t.type({
  id: t.string,
  recipientId: t.string,
  source: fromEnum<Source>("Source", Source),
  paymentDate: t.number,
  amount: t.number,
  firstName: t.string,
  lastName: t.string,
  addressLine1: t.string,
  city: t.string,
  state: fromEnum<State>("State", State),
  country: t.string,
  postalCode: t.string,
  entityType: fromEnum<EntityType>("EntityType", EntityType),
  paymentMethod: fromEnum<PaymentMethod>("PaymentMethod", PaymentMethod),
});

const ProcessorFeeDataReq = t.type({
  amount: t.number,
  entityName: t.string,
  paymentDate: t.number,
  addressLine1: t.string,
  city: t.string,
  state: fromEnum<State>("State", State),
  paymentMethod: fromEnum<PaymentMethod>("PaymentMethod", PaymentMethod),
  postalCode: t.string,
  country: t.string,
});

const ProcessorFeeDataOpt = t.partial({
  addressLine2: t.string,
  checkNumber: t.string,
  payoutId: t.string,
});

export const ProcessorFeeData = t.intersection([
  ProcessorFeeDataReq,
  ProcessorFeeDataOpt,
]);

export type IProcessorFeeData = t.TypeOf<typeof ProcessorFeeData>;

const ExternalContribOpt = t.partial({
  payoutDate: t.number,
  payoutId: t.string,
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
  checkNumber: t.string,
  processorFeeData: ProcessorFeeData,
});

export const ExternalContrib = t.intersection([
  ExternalContribReq,
  ExternalContribOpt,
]);

export type IExternalContrib = t.TypeOf<typeof ExternalContrib>;

export type ContributionMapper = (a: any) => IExternalContrib;

export type IsNewValidator = (
  transactionTable: string
) => (
  dynamoDB: DynamoDB
) => (
  committeeId: string
) => (txnId: string) => TaskEither<ApplicationError, boolean>;

export type CommitteeGetter = (
  committeeTable: string
) => (
  dynamoDB: DynamoDB
) => (recipientId: string) => TaskEither<ApplicationError, ICommittee>;

export type CommitteeValidator = (
  committee: ICommittee
) => (externalAccountId: string) => boolean;

export interface IExternalTxnsToDDBDeps {
  committeesTable: string;
  billableEventsTable: string;
  donorsTable: string;
  transactionsTable: string;
  rulesTable: string;
  dynamoDB: DynamoDB;
  stripe: Stripe;
  lexisNexisConfig: ILexisNexisConfig;
  committeeValidator: CommitteeValidator;
  contributionMapper: ContributionMapper;
}
