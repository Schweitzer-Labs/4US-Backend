import { pipe } from "fp-ts/function";
import { DynamoDB } from "aws-sdk";
import { taskEither } from "fp-ts";
import { right, TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { ICommittee } from "../queries/get-committee-by-id.query";
import * as FPArray from "fp-ts/lib/Array";

import {
  FinicityConfig,
  IFinicityTransaction,
} from "../clients/finicity/finicity.decoders";
import { ITransaction } from "../queries/search-transactions.decoder";
import { getTransactions } from "../clients/finicity/finicity.client";
import { epochToMilli, milliToEpoch, now } from "../utils/time.utils";
import { searchTransactions } from "../queries/search-transactions.query";
import { getAll4USCommittees } from "../utils/model/get-all-4us-committees.utils";
import { Direction } from "../utils/enums/direction.enum";
import { genTxnId } from "../utils/gen-txn-id.utils";
import { Source } from "../utils/enums/source.enum";
import { PaymentMethod } from "../utils/enums/payment-method.enum";
import { TransactionType } from "../utils/enums/transaction-type.enum";
import { FinicityTransactionType } from "../utils/enums/finicity-transaction-type.enum";
import { putTransactionAndDecode } from "../utils/model/put-transaction.utils";

export const finicityBankSync =
  (config: FinicityConfig) =>
  (txnsTable: string) =>
  (committeesTable: string) =>
  (dynamoDB: DynamoDB) =>
    pipe(
      getAll4USCommittees(committeesTable)(dynamoDB),
      taskEither.map(
        FPArray.map((committee) =>
          pipe(
            taskEither.of(committee),
            taskEither.chain(getAllFinicityTxns(config)),
            taskEither.chain((finicityTxns) =>
              pipe(
                getUnverifiedTxns(txnsTable)(dynamoDB)(committee),
                taskEither.chain((platformTxns) =>
                  pipe(
                    finicityTxns,
                    FPArray.map(
                      matchAndProcess(txnsTable)(dynamoDB)(committee)(
                        platformTxns
                      )
                    ),
                    taskEither.of
                  )
                )
              )
            )
          )
        )
      )
    );

const matchToPlatformTxn =
  (pTxns: ITransaction[]) =>
  (fTxn: ITransaction): ITransaction[] => {
    return pTxns.filter((pTxn) => {
      const isFinicityTxn =
        pTxn.finicityTransactionId === fTxn.finicityTransactionId;
      return isFinicityTxn;
    });
  };

const matchAndProcess =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committee: ICommittee) =>
  (platformTxns: ITransaction[]) =>
  (
    finicityTxn: IFinicityTransaction
  ): TaskEither<ApplicationError, ITransaction> => {
    const fTxn = finicityTxnToPlatformTxn(committee)(finicityTxn);
    const pTxnRes = matchToPlatformTxn(platformTxns)(fTxn);

    switch (pTxnRes.length) {
      case 1:
        const [txn] = pTxnRes;
        return putTransactionAndDecode(txnsTableName)(dynamoDB)({
          ...txn,
          bankVerified: true,
          finicityTransactionId: fTxn.finicityTransactionId,
          finicityTransactionData: finicityTxn,
        });
      case 0:
        return putTransactionAndDecode(txnsTableName)(dynamoDB)(fTxn);
      default:
        return taskEither.left(
          new ApplicationError(
            "Unhandled duplicates found",
            JSON.stringify(pTxnRes)
          )
        );
    }
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
): TransactionType => {
  if (fTxn.categorization.normalizedPayeeName === "deposit") {
    return TransactionType.Deposit;
  } else if (fTxn.amount < 0) {
    return TransactionType.Disbursement;
  }
};

const finicityTxnToPlatformTxn =
  (committee: ICommittee) =>
  (fTxn: IFinicityTransaction): ITransaction => {
    return {
      committeeId: committee.id,
      id: genTxnId(),
      amount: Math.round(Math.abs(fTxn.amount)) * 100,
      paymentMethod: finicityTxnToPaymentMethod(fTxn),
      direction: fTxn.amount > 0 ? Direction.In : Direction.Out,
      paymentDate: epochToMilli(fTxn.postedDate),
      initiatedTimestamp: epochToMilli(fTxn.postedDate),
      source: Source.FINICITY,
      bankVerified: true,
      ruleVerified: false,
      checkNumber: fTxn.checkNum + "",
      transactionType: finicityTxnToTransactionType(fTxn),
      finicityTransactionId: fTxn.id,
      finicityTransactionData: fTxn,
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

    const epochFrom = milliToEpoch(now()) - 60 * 60 * 24 * 30 * 6;
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
