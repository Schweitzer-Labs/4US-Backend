import { DynamoDB } from "aws-sdk";
import { ICommittee } from "../model/committee.type";
import { pipe } from "fp-ts/function";
import {
  IExternalContrib,
  IExternalTxnsToDDBDeps,
  IsNewValidator,
} from "../model/external-data.type";
import { ApplicationError } from "../utils/application-error";
import { TaskEither } from "fp-ts/TaskEither";
import { taskEither } from "fp-ts";
import { CreateContributionInput } from "../graphql/input-types/create-contribution.input-type";
import { getOneFromList } from "../utils/get-one-from-list.utils";
import { PaymentMethod } from "../utils/enums/payment-method.enum";
import { EntityType } from "../utils/enums/entity-type.enum";
import { runRulesAndProcess } from "./run-rules-and-process.pipe";
import { Stripe } from "stripe";
import { ILexisNexisConfig } from "../clients/lexis-nexis/lexis-nexis.client";
import { SYSTEM } from "../utils/tokens/users.token";
import * as Array from "fp-ts/lib/Array";
import { ITransaction } from "../model/transaction.type";
import { genTxnId } from "../utils/gen-txn-id.utils";
import { Direction } from "../utils/enums/direction.enum";
import { Source } from "../utils/enums/source.enum";
import { TransactionType } from "../utils/enums/transaction-type.enum";
import { PurposeCode } from "../utils/enums/purpose-code.enum";
import { putTransactionAndDecode } from "../utils/model/transaction/put-transaction.utils";
import { mLog } from "../utils/m-log.utils";

export const syncExternalContributions =
  ({
    committeesTable,
    billableEventsTable,
    donorsTableName,
    transactionsTableName,
    rulesTableName,
    dynamoDB,
    stripe,
    lexisNexisConfig,
    committeeGetter,
    isNewValidator,
    contributionMapper,
  }: IExternalTxnsToDDBDeps) =>
  (data: any): TaskEither<ApplicationError, IExternalContrib[]> =>
    pipe(
      taskEither.of(data.map(contributionMapper)),
      taskEither.chain((contributions) =>
        pipe(
          getOneFromList<IExternalContrib>(contributions),
          taskEither.map((extContrib) => extContrib.recipientId),
          taskEither.chain(committeeGetter(committeesTable)(dynamoDB)),
          taskEither.chain(
            syncContribs(isNewValidator)(billableEventsTable)(donorsTableName)(
              transactionsTableName
            )(rulesTableName)(dynamoDB)(stripe)(lexisNexisConfig)(contributions)
          )
        )
      )
    );

const syncContribs =
  (isNewValidator: IsNewValidator) =>
  (billableEventsTableName: string) =>
  (donorsTableName: string) =>
  (txnsTableName: string) =>
  (rulesTableName: string) =>
  (ddb: DynamoDB) =>
  (stripe: Stripe) =>
  (lnConfig: ILexisNexisConfig) =>
  (inputs: IExternalContrib[]) =>
  (committee: ICommittee): TaskEither<ApplicationError, IExternalContrib[]> =>
    Array.traverse(taskEither.ApplicativeSeq)(
      syncContrib(isNewValidator)(billableEventsTableName)(donorsTableName)(
        txnsTableName
      )(rulesTableName)(ddb)(stripe)(lnConfig)(committee)
    )(inputs);

const syncContrib =
  (isNewValidator: IsNewValidator) =>
  (billableEventsTableName: string) =>
  (donorsTableName: string) =>
  (txnsTableName: string) =>
  (rulesTableName: string) =>
  (ddb: DynamoDB) =>
  (stripe: Stripe) =>
  (lnConfig: ILexisNexisConfig) =>
  (committee: ICommittee) =>
  (
    extContrib: IExternalContrib
  ): TaskEither<ApplicationError, IExternalContrib> =>
    pipe(
      isNewValidator(txnsTableName)(ddb)(committee.id)(extContrib.id),
      taskEither.chain((isNew) =>
        isNew
          ? pipe(
              taskEither.of(
                extContribToCreateContribInput(committee)(extContrib)
              ),
              taskEither.chain(
                runRulesAndProcess(true)(billableEventsTableName)(
                  donorsTableName
                )(txnsTableName)(rulesTableName)(ddb)(stripe)(lnConfig)(SYSTEM)(
                  committee
                )
              ),
              taskEither.map(externalContribAndTxnToFeeTxn(extContrib)),
              taskEither.chain(putTransactionAndDecode(txnsTableName)(ddb)),
              taskEither.chain(mLog("Transaction put")),
              taskEither.map(() => extContrib)
            )
          : taskEither.of(extContrib)
      )
    );

const extContribToCreateContribInput =
  (com: ICommittee) =>
  (extContrib: IExternalContrib): CreateContributionInput => ({
    processPayment: false,
    committeeId: com.id,
    paymentMethod: PaymentMethod.Credit,
    paymentDate: extContrib.paymentDate,
    amount: extContrib.amount,
    firstName: extContrib.firstName,
    middleName: extContrib.middleName,
    lastName: extContrib.lastName,
    addressLine1: extContrib.addressLine1,
    addressLine2: extContrib.addressLine2,
    city: extContrib.city,
    state: extContrib.state,
    postalCode: extContrib.postalCode,
    entityType: EntityType.Ind,
    emailAddress: extContrib.emailAddress,
    employer: extContrib.employer,
    employmentStatus: extContrib.employmentStatus,
    occupation: extContrib.occupation,
    phoneNumber: extContrib.phoneNumber,
    source: Source.ActBlue,
  });

const externalContribAndTxnToFeeTxn =
  (c: IExternalContrib) =>
  (txn: ITransaction): ITransaction => ({
    id: genTxnId(),
    source: Source.ActBlue,
    ...c.processorFeeData,
    committeeId: txn.committeeId,
    direction: Direction.Out,
    paymentDate: c.processorFeeData.paymentDate,
    bankVerified: false,
    ruleVerified: true,
    initiatedTimestamp: c.paymentDate,
    transactionType: TransactionType.Disbursement,
    paymentMethod: PaymentMethod.Check,
    isSubcontracted: false,
    isPartialPayment: false,
    isExistingLiability: false,
    purposeCode: PurposeCode.FUNDR,
    checkNumber: c.checkNumber,
  });
