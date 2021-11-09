import { DynamoDB } from "aws-sdk";
import { ICommittee } from "../model/committee.type";
import { pipe } from "fp-ts/function";
import { IExternalData, IExternalTxn } from "../model/external-data.type";
import { ApplicationError } from "../utils/application-error";
import { TaskEither } from "fp-ts/TaskEither";
import { taskEither } from "fp-ts";
import { CreateContributionInput } from "../graphql/input-types/create-contribution.input-type";
import { getCommitteeByActBlueIdAndDecode } from "../utils/model/committee/get-committee-by-actblue-id.utils";
import { getOneFromList } from "../utils/get-one-from-list.utils";
import { PaymentMethod } from "../utils/enums/payment-method.enum";
import { EntityType } from "../utils/enums/entity-type.enum";
import { isNewActBlueTxn } from "../utils/model/transaction/get-txn-by-actblue-id.utils";
import { runRulesAndProcess } from "./run-rules-and-process.pipe";
import { Stripe } from "stripe";
import { ILexisNexisConfig } from "../clients/lexis-nexis/lexis-nexis.client";
import { ANONYMOUS, SYSTEM } from "../utils/tokens/users.token";
import * as Array from "fp-ts/lib/Array";

const extTxnToCreateContribInput =
  (com: ICommittee) =>
  (extTxn: IExternalTxn): CreateContributionInput => ({
    processPayment: false,
    actBlueTransactionId: extTxn.id,
    committeeId: com.id,
    paymentMethod: PaymentMethod.Credit,
    paymentDate: extTxn.paymentDate,
    amount: extTxn.amount,
    firstName: extTxn.firstName,
    middleName: extTxn.middleName,
    lastName: extTxn.lastName,
    addressLine1: extTxn.addressLine1,
    addressLine2: extTxn.addressLine2,
    city: extTxn.city,
    state: extTxn.state,
    postalCode: extTxn.postalCode,
    entityType: EntityType.Ind,
    emailAddress: extTxn.emailAddress,
    employer: extTxn.employer,
    employmentStatus: extTxn.employmentStatus,
    occupation: extTxn.occupation,
    phoneNumber: extTxn.phoneNumber,
  });

const externalDataToCreateContribInput =
  (extTxns: IExternalTxn[]) =>
  (com: ICommittee): CreateContributionInput[] =>
    extTxns.map(extTxnToCreateContribInput(com));

export const externalTxnsToDdb =
  (comTable: string) =>
  (billableEventsTableName: string) =>
  (donorsTableName: string) =>
  (txnsTableName: string) =>
  (rulesTableName: string) =>
  (ddb: DynamoDB) =>
  (stripe: Stripe) =>
  (lnConfig: ILexisNexisConfig) =>
  ({
    transactions,
  }: IExternalData): TaskEither<ApplicationError, CreateContributionInput[]> =>
    pipe(
      getOneFromList<IExternalTxn>(transactions),
      taskEither.map((extTxn) => extTxn.recipientId),
      taskEither.chain(getCommitteeByActBlueIdAndDecode(comTable)(ddb)),
      taskEither.chain((com) =>
        pipe(
          taskEither.of(com),
          taskEither.map(externalDataToCreateContribInput(transactions)),
          taskEither.chain(
            syncTxns(billableEventsTableName)(donorsTableName)(txnsTableName)(
              rulesTableName
            )(ddb)(stripe)(lnConfig)(com)
          )
        )
      )
    );

const syncTxns =
  (billableEventsTableName: string) =>
  (donorsTableName: string) =>
  (txnsTableName: string) =>
  (rulesTableName: string) =>
  (ddb: DynamoDB) =>
  (stripe: Stripe) =>
  (lnConfig: ILexisNexisConfig) =>
  (committee: ICommittee) =>
  (
    inputs: CreateContributionInput[]
  ): TaskEither<ApplicationError, CreateContributionInput[]> =>
    Array.traverse(taskEither.ApplicativeSeq)(
      syncTxn(billableEventsTableName)(donorsTableName)(txnsTableName)(
        rulesTableName
      )(ddb)(stripe)(lnConfig)(committee)
    )(inputs);

const syncTxn =
  (billableEventsTableName: string) =>
  (donorsTableName: string) =>
  (txnsTableName: string) =>
  (rulesTableName: string) =>
  (ddb: DynamoDB) =>
  (stripe: Stripe) =>
  (lnConfig: ILexisNexisConfig) =>
  (committee: ICommittee) =>
  (
    input: CreateContributionInput
  ): TaskEither<ApplicationError, CreateContributionInput> =>
    pipe(
      isNewActBlueTxn(txnsTableName)(ddb)({
        committeeId: input.committeeId,
        actBlueTransactionId: input.actBlueTransactionId,
      }),
      taskEither.chain((isNew) =>
        isNew
          ? taskEither.of(input)
          : pipe(
              runRulesAndProcess(true)(billableEventsTableName)(
                donorsTableName
              )(txnsTableName)(rulesTableName)(ddb)(stripe)(lnConfig)(SYSTEM)(
                committee
              )(input),
              taskEither.map(() => input)
            )
      )
    );
