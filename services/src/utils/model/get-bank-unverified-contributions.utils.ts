import { DynamoDB } from "aws-sdk";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../application-error";
import { ITransaction } from "../../types/transaction.type";
import { searchTransactions } from "../../queries/search-transactions.query";
import { TransactionType } from "../enums/transaction-type.enum";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { ICommittee } from "../../types/committee.type";

export const get_bank_unverified_contributions_not_paid_out =
  (txnsTable: string) =>
  (dynamoDB: DynamoDB) =>
  (committee: ICommittee): TaskEither<ApplicationError, ITransaction[]> =>
    pipe(
      searchTransactions(txnsTable)(dynamoDB)({
        committeeId: committee.id,
        transactionType: TransactionType.Contribution,
        bankVerified: false,
      }),
      taskEither.map((txns) => txns.filter((txn) => !txn.stripePayoutId))
    );
