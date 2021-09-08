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
import { putTransactionAndDecode } from "../../utils/model/put-transaction.utils";

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
      taskEither.chain(takeOne),
      taskEither.map(toReconciled),
      taskEither.chain(putTransactionAndDecode(txnTable)(ddb))
    );

const toReconciled = (txn: ITransaction): ITransaction => ({
  ...txn,
  finicityBestRepresentation:
    "ORIG CO NAME US ORIG ID DESC DATE CO ENTRY DESCR US SEC CCD TRACE EED IND ID ST IND NAME SCHWEITZER LABORATORIE TRN TC",
  finicityCategory: "Income",
  finicityDescription: "ORIG CO NAME:4US",
  finicityNormalizedPayeeName: "Co Desc",
  finicityPaymentMethod: "Credit",
  finicityPostedDate: txn.paymentDate,
  finicityTransactionDate: txn.paymentDate,
});

const takeOne = (
  txns: ITransaction[]
): TaskEither<ApplicationError, ITransaction> =>
  txns.length > 0
    ? taskEither.right(txns[0])
    : taskEither.left(
        new ApplicationError("Demo: There are no unverified contributions", {})
      );
