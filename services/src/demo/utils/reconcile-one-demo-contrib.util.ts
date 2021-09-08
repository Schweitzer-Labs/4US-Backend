import { searchTransactions } from "../../queries/search-transactions.query";
import { Order } from "../../utils/enums/order.enum";
import { TransactionType } from "../../utils/enums/transaction-type.enum";
import { DynamoDB } from "aws-sdk";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";
import { ITransaction } from "../../queries/search-transactions.decoder";
import { taskEither } from "fp-ts";
import { validateDemoCommittee } from "./validate-demo-committee.utils";
import { pipe } from "fp-ts/function";

export const reconcileOneDemoContrib =
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (committeeId: string): TaskEither<ApplicationError, ITransaction> =>
    pipe(
      validateDemoCommittee(committeeId),
      taskEither.chain(() =>
        searchTransactions(txnTable)(ddb)({
          committeeId,
          order: Order.Asc,
          transactionType: TransactionType.Contribution,
          bankVerified: false,
          ruleVerified: true,
        })
      ),
      taskEither.chain(takeOne)
    );

const takeOne = (
  txns: ITransaction[]
): TaskEither<ApplicationError, ITransaction> =>
  txns.length > 0
    ? taskEither.right(txns[0])
    : taskEither.left(
        new ApplicationError("Demo: There are no unverified contributions", {})
      );
