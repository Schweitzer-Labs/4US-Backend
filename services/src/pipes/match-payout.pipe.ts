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

export const matchPayout =
  (txnsTableName: string) =>
  (committeesTableName: string) =>
  (dynamoDB: DynamoDB) =>
    pipe(
      getAll4USCommitteesAndDecode(committeesTableName)(dynamoDB),
      te.map(FPArray.map(matchContributions(txnsTableName)(dynamoDB)))
    );

export const matchContributions =
  (txnsTableName: string) => (dynamoDB: DynamoDB) => (committee: ICommittee) =>
    pipe(
      getCommitteeContributions(txnsTableName)(dynamoDB)(committee),
      te.chain(mProcessTxns(txnsTableName)(dynamoDB))
    );

const getCommitteeContributions =
  (txnsTableName) => (dynamoDB) => (committee: ICommittee): TaskEither<ApplicationError, ITransaction[]> =>
    searchTransactions(txnsTableName)(dynamoDB)({
      committeeId: committee.id,
      transactionType: TransactionType.Contribution,
      bankVerified: false,
    });


interface PayoutRecord {
  stripePayoutId: string,
  stripeBalanceTransactionId: string,
  stripeEffectiveAt: number,
}

const processTxns =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
    (payoutRecords: PayoutRecord[]) => async (txns: ITransaction[]): Promise<ITransaction[]> =>
    txns.reduce((acc, txn) => {
      const match = []

      return [
        ..acc,

      ]
    }, [])
