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
  entityType: fromEnum<EntityType>("EntityType", EntityType),
});

const ExternalContribOpt = t.partial({
  payoutDate: t.number,
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

export type ContributionMapper<a> = (a: a) => IExternalContrib;

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

export interface IExternalTxnsToDDBDeps<schema> {
  committeesTable: string;
  billableEventsTable: string;
  donorsTableName: string;
  transactionsTableName: string;
  rulesTableName: string;
  dynamoDB: DynamoDB;
  stripe: Stripe;
  lexisNexisConfig: ILexisNexisConfig;
  committeeGetter: CommitteeGetter;
  contributionMapper: ContributionMapper<schema>;
  isNewValidator: IsNewValidator;
}

export const toDeps =
  <schema>(committeeGetter: CommitteeGetter) =>
  (contributionMapper: ContributionMapper<schema>) =>
  (isNewValidator: IsNewValidator) =>
  (committeesTable: string) =>
  (billableEventsTable: string) =>
  (donorsTableName: string) =>
  (transactionsTableName: string) =>
  (rulesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (stripe: Stripe) =>
  (lexisNexisConfig: ILexisNexisConfig): IExternalTxnsToDDBDeps<schema> => ({
    committeesTable,
    billableEventsTable,
    donorsTableName,
    transactionsTableName,
    rulesTableName,
    dynamoDB,
    stripe,
    lexisNexisConfig,
    committeeGetter,
    contributionMapper,
    isNewValidator,
  });
