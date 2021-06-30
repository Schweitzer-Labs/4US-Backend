import { DynamoDB } from "aws-sdk";
import { ICommittee } from "../queries/get-committee-by-id.query";
import { flow, pipe } from "fp-ts/function";
import { taskEither as te } from "fp-ts";
import { searchTransactions } from "../queries/search-transactions.query";
import { TransactionType } from "../utils/enums/transaction-type.enum";
import { ITransaction } from "../queries/search-transactions.decoder";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { putTransaction } from "../utils/model/put-transaction.utils";
import { deleteTxn } from "../utils/model/delete-txn.utils";
import { Direction } from "../utils/enums/direction.enum";
import { payoutDescriptions } from "../clients/finicity/finicity.decoders";
import { getAll4USCommitteesAndDecode } from "../utils/model/get-all-4us-committees.utils";
import * as FPArray from "fp-ts/Array";

export const reconcileAll =
  (txnsTableName: string) =>
  (committeesTableName: string) =>
  (dynamoDB: DynamoDB) =>
    pipe(
      getAll4USCommitteesAndDecode(committeesTableName)(dynamoDB),
      te.map(
        FPArray.map(reconcileCommitteeContributions(txnsTableName)(dynamoDB))
      )
    );

export const reconcileCommitteeContributions =
  (txnsTableName: string) => (dynamoDB: DynamoDB) => (committee: ICommittee) =>
    pipe(
      getCommitteeContributions(txnsTableName)(dynamoDB)(committee),
      te.chain(mProcessTxns(txnsTableName)(dynamoDB))
    );

const getCommitteeContributions =
  (txnsTableName) => (dynamoDB) => (committee: ICommittee) =>
    searchTransactions(txnsTableName)(dynamoDB)({
      committeeId: committee.id,
      transactionType: TransactionType.Contribution,
    });

const mProcessTxns =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (txns: ITransaction[]): TaskEither<ApplicationError, ITransaction[]> =>
    te.tryCatch(
      () => processTxns(txnsTableName)(dynamoDB)(txns),
      (e) => new ApplicationError("Contribution reconcile failed", e)
    );

const processTxns =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (txns: ITransaction[]): Promise<ITransaction[]> => {
    let currentPayout: ITransaction;
    let acc: ITransaction[] = [];
    let reconciledTxns: ITransaction[] = [];

    for (const t of txns) {
      if (isPayout(t)) {
        if (sumTxns(acc) === currentPayout?.amount) {
          for (const matchedTxns of acc) {
            const verifiedTxn = markTxnAsVerifiedWithFinicity(t)(matchedTxns);
            const savedTxn = await putTransaction(txnsTableName)(dynamoDB)(
              verifiedTxn
            );
            reconciledTxns.push(savedTxn);
          }
          await deleteTxn(txnsTableName)(dynamoDB)(t);
          acc = [];
        }
      } else if (isUnverifiedContribution(t)) {
        acc.push(t);
      }
    }

    return reconciledTxns;
  };

const isUnverifiedContribution = (txn: ITransaction) =>
  txn.transactionType === TransactionType.Contribution && !txn.bankVerified;

export const isPayout = (txn: ITransaction) =>
  txn.direction === Direction.In &&
  payoutDescriptions.includes(txn?.finicityTransactionData?.description);

const sumTxns = (txns: ITransaction[]) =>
  txns.reduce((acc, { amount }) => acc + amount, 0);

const markTxnAsVerifiedWithFinicity =
  ({ finicityTransactionId, finicityTransactionData }: ITransaction) =>
  (txn: ITransaction): ITransaction => ({
    ...txn,
    finicityTransactionId,
    finicityTransactionData,
  });
