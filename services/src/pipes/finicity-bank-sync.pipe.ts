import { pipe } from "fp-ts/function";
import { DynamoDB, SQS } from "aws-sdk";
import { taskEither } from "fp-ts";
import { right, TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import * as FPArray from "fp-ts/lib/Array";

import {
  FinicityConfig,
  IFinicityTransaction,
} from "../clients/finicity/finicity.decoders";
import { ITransaction, Transactions } from "../types/transaction.type";
import { getTransactions } from "../clients/finicity/finicity.client";
import { epochToMilli, milliToEpoch, now } from "../utils/time.utils";
import { searchTransactions } from "../queries/search-transactions.query";
import { Direction } from "../utils/enums/direction.enum";
import { dateToTxnId } from "../utils/gen-txn-id.utils";
import { Source } from "../utils/enums/source.enum";
import { PaymentMethod } from "../utils/enums/payment-method.enum";
import { TransactionType } from "../utils/enums/transaction-type.enum";
import { FinicityTransactionType } from "../utils/enums/finicity-transaction-type.enum";
import {
  putTransaction,
  putTransactionAndDecode,
} from "../utils/model/put-transaction.utils";
import { decodeRawData } from "../utils/decode-raw-data.util";
import { ICommittee } from "../types/committee.type";

export const syncCommittee =
  (config: FinicityConfig) =>
  (txnsTable: string) =>
  (ddb: DynamoDB) =>
  (committee: ICommittee): TaskEither<ApplicationError, ITransaction[]> =>
    pipe(
      taskEither.of(committee),
      taskEither.chain(getAllFinicityTxns(config)),
      taskEither.chain((finicityTxns: IFinicityTransaction[]) =>
        pipe(
          getUnverifiedTxns(txnsTable)(ddb)(committee),
          taskEither.chain(
            syncFTxnsAndDecode(txnsTable)(ddb)(committee)(finicityTxns)
          )
        )
      )
    );

const syncFTxnsAndDecode =
  (txnsTable: string) =>
  (ddb: DynamoDB) =>
  (committee: ICommittee) =>
  (finicityTxns: IFinicityTransaction[]) =>
  (
    platformTxns: ITransaction[]
  ): TaskEither<ApplicationError, ITransaction[]> =>
    pipe(
      taskEither.tryCatch(
        () => syncFTxns(txnsTable)(ddb)(committee)(finicityTxns)(platformTxns),
        (err) => new ApplicationError("Finicity transactions sync failed", err)
      ),
      taskEither.chain(
        decodeRawData("New platform transactions from finicity")(Transactions)
      )
    );

const syncFTxns =
  (txnsTable: string) =>
  (ddb: DynamoDB) =>
  (committee: ICommittee) =>
  (finicityTxns: IFinicityTransaction[]) =>
  async (platformTxns: ITransaction[]): Promise<ITransaction[]> => {
    console.log("Committee for current run", committee.id);
    const newTxns = [];
    for (const finicityTxn of finicityTxns) {
      const fTxn = finicityTxnToPlatformTxn(committee)(finicityTxn);
      const pTxnRes = matchToPlatformTxn(platformTxns)(fTxn);
      if (pTxnRes.length === 0) {
        let newTxn = await putTransaction(txnsTable)(ddb)(fTxn);
        newTxns.push(newTxn);
      }
    }
    return newTxns;
  };

const matchToPlatformTxn =
  (pTxns: ITransaction[]) =>
  (fTxn: ITransaction): ITransaction[] => {
    return pTxns.filter(
      (pTxn) => pTxn.finicityTransactionId === fTxn.finicityTransactionId
    );
  };

const finicityTxnToPaymentMethod = (
  fTxn: IFinicityTransaction
): PaymentMethod => {
  switch (fTxn.type) {
    case FinicityTransactionType.cash:
      return PaymentMethod.Cash;
    case FinicityTransactionType.debit:
      return PaymentMethod.Debit;
    case FinicityTransactionType.check:
      return PaymentMethod.Check;
    case FinicityTransactionType.transfer:
      return PaymentMethod.Transfer;
    case FinicityTransactionType.credit:
      return PaymentMethod.Credit;
    default:
      return PaymentMethod.Other;
  }
};

const finicityTxnToTransactionType = (
  fTxn: IFinicityTransaction
): TransactionType =>
  fTxn.amount > 0 ? TransactionType.Contribution : TransactionType.Disbursement;

const finicityTxnToPlatformTxn =
  (committee: ICommittee) =>
  (fTxn: IFinicityTransaction): ITransaction => {
    const amount = Math.round(Math.abs(fTxn.amount) * 100);

    const checkNumber = fTxn.checkNum
      ? { checkNumber: fTxn.checkNum + "" }
      : {};

    const paymentDate = epochToMilli(fTxn.postedDate);
    return {
      entityName: fTxn.categorization.normalizedPayeeName,
      committeeId: committee.id,
      id: dateToTxnId(paymentDate),
      amount,
      paymentMethod: finicityTxnToPaymentMethod(fTxn),
      direction: fTxn.amount > 0 ? Direction.In : Direction.Out,
      paymentDate,
      initiatedTimestamp: epochToMilli(fTxn.transactionDate),
      source: Source.FINICITY,
      bankVerified: true,
      ruleVerified: false,
      ...checkNumber,
      transactionType: finicityTxnToTransactionType(fTxn),
      finicityTransactionId: fTxn.id,
      finicityTransactionData: fTxn,
      finicityNormalizedPayeeName: fTxn.categorization.normalizedPayeeName,
      finicityDescription: fTxn.description,
      finicityBestRepresentation: fTxn.categorization.bestRepresentation,
      finicityCategory: fTxn.categorization.category,
      finicityPostedDate: paymentDate,
      finicityTransactionDate: epochToMilli(fTxn.transactionDate),
      finicityPaymentMethod: finicityTxnToPaymentMethod(fTxn),
    };
  };

const getAllFinicityTxns =
  (config: FinicityConfig) =>
  (
    committee: ICommittee
  ): TaskEither<ApplicationError, IFinicityTransaction[]> => {
    const { finicityAccountId, finicityCustomerId } = committee;

    if (!finicityAccountId || !finicityCustomerId) {
      return right([]);
    }

    const epochFrom = milliToEpoch(now()) - 60 * 60 * 24 * 30 * 8;
    const epochTo = milliToEpoch(now());
    return getTransactions(config)({
      customerId: finicityCustomerId,
      accountId: finicityAccountId,
      epochFrom,
      epochTo,
    });
  };

const getUnverifiedTxns =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committee: ICommittee): TaskEither<ApplicationError, ITransaction[]> =>
    searchTransactions(txnsTableName)(dynamoDB)({
      committeeId: committee.id,
    });
