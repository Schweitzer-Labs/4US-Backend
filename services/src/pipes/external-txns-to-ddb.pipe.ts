import { DynamoDB } from "aws-sdk";
import { ICommittee } from "../model/committee.type";
import { pipe } from "fp-ts/function";
import {
  CommitteeGetter,
  ContributionMapper,
  IExternalContrib,
  IExternalData,
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

interface IExternalTxnsToDDBDeps {
  committeesTable: string;
  billableEventsTable: string;
  donorsTableName: string;
  transactionsTableName: string;
  rulesTableName: string;
  dynamoDB: DynamoDB;
  stripe: Stripe;
  lexisNexisConfig: ILexisNexisConfig;
  committeeGetter: CommitteeGetter;
  contributionMapper: ContributionMapper;
}

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
    contributionMapper,
  }: IExternalTxnsToDDBDeps) =>
  ({
    contributions,
  }: IExternalData): TaskEither<ApplicationError, IExternalContrib[]> =>
    pipe(
      getOneFromList<IExternalContrib>(contributions),
      taskEither.map((extContrib) => extContrib.recipientId),
      taskEither.chain(committeeGetter(committeesTable)(dynamoDB)),
      taskEither.chain(
        syncContribs(contributionMapper)(billableEventsTable)(donorsTableName)(
          transactionsTableName
        )(rulesTableName)(dynamoDB)(stripe)(lexisNexisConfig)(contributions)
      )
    );

const syncContribs =
  (contribMapper: ContributionMapper) =>
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
      syncContrib(contribMapper)(billableEventsTableName)(donorsTableName)(
        txnsTableName
      )(rulesTableName)(ddb)(stripe)(lnConfig)(committee)
    )(inputs);

const syncContrib =
  (contribMapper: ContributionMapper) =>
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
      isNewActBlueTxn(txnsTableName)(ddb)({
        committeeId: committee.id,
        actBlueTransactionId: extContrib.id,
      }),
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

// const externalContribAndTxnToFeeTxn =
//   (extContrib: IExternalContrib) =>
//   (txn: ITransaction): ITransaction => ({
//     committeeId: txn.committeeId,
//     amount: dollarStrToCents(extContrib.fee),
//     id: genTxnId(),
//     direction: Direction.Out,
//     paymentDate:
//     bankVerified: false,
//     ruleVerified: true,
//     initiatedTimestamp: now(),
//     source: Source.ActBlue,
//     transactionType: TransactionType.Disbursement,
//   });
