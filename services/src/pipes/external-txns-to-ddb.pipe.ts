import { DynamoDB } from "aws-sdk";
import { ICommittee } from "../model/committee.type";
import { pipe } from "fp-ts/function";
import {
  CommitteeGetter,
  ContributionMapper,
  IExternalContrib,
  IExternalData,
  IExternalTxnsToDDBDeps,
  IsNewValidator,
} from "../model/external-data.type";
import { ApplicationError } from "../utils/application-error";
import { TaskEither } from "fp-ts/TaskEither";
import { taskEither } from "fp-ts";
import { CreateContributionInput } from "../graphql/input-types/create-contribution.input-type";
import { getCommitteeByActBlueAccountIdAndDecode } from "../utils/model/committee/get-committee-by-actblue-id.utils";
import { getOneFromList } from "../utils/get-one-from-list.utils";
import { PaymentMethod } from "../utils/enums/payment-method.enum";
import { EntityType } from "../utils/enums/entity-type.enum";
import { isNewActBlueTxn } from "../utils/model/transaction/get-txn-by-actblue-id.utils";
import { runRulesAndProcess } from "./run-rules-and-process.pipe";
import { Stripe } from "stripe";
import { ILexisNexisConfig } from "../clients/lexis-nexis/lexis-nexis.client";
import { ANONYMOUS, SYSTEM } from "../utils/tokens/users.token";
import * as Array from "fp-ts/lib/Array";
import { ITransaction } from "../model/transaction.type";
import { genTxnId } from "../utils/gen-txn-id.utils";
import { Direction } from "../utils/enums/direction.enum";
import { now } from "../utils/time.utils";
import { Source } from "../utils/enums/source.enum";
import { TransactionType } from "../utils/enums/transaction-type.enum";
import { formatDate } from "../clients/actblue/actblue.decoders";
import { PurposeCode } from "../utils/enums/purpose-code.enum";

export const syncExternalContributions =
  <schema>({
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
  }: IExternalTxnsToDDBDeps<schema>) =>
  (data: schema[]): TaskEither<ApplicationError, IExternalContrib[]> =>
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
  });

const externalContribAndTxnToFeeTxn =
  (c: IExternalContrib) =>
  (txn: ITransaction): ITransaction => ({
    id: genTxnId(),
    source: Source.ActBlue,
    amount: c.processorFee,
    entityName: c.processorEntityName,
    addressLine1: c.processorEntityName,
    addressLine2: c.processorEntityName,
    city: c.processorCity,
    state: c.processorState,
    postalCode: c.processorPostalCode,
    committeeId: txn.committeeId,
    direction: Direction.Out,
    paymentDate: c.payoutDate,
    bankVerified: false,
    ruleVerified: true,
    initiatedTimestamp: now(),
    transactionType: TransactionType.Disbursement,
    paymentMethod: c.paymentMethod,
    isSubcontracted: false,
    isPartialPayment: false,
    isExistingLiability: false,
    purposeCode: PurposeCode.FUNDR,
    checkNumber: c.checkNumber,
  });
