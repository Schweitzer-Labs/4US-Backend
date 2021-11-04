import { DynamoDB } from "aws-sdk";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { IAggs } from "../types/aggs.type";
import { pipe } from "fp-ts/function";
import { searchTransactions } from "../queries/search-transactions.query";
import { taskEither } from "fp-ts";
import { txnsToAgg } from "../utils/model/txns-to-agg.utils";
import { putAggAndDecode } from "../utils/model/put-aggs.utils";
import { ITransaction } from "../types/transaction.type";

export const refreshAggsFromTxn =
  (aggTable: string) =>
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (txn: ITransaction): TaskEither<ApplicationError, IAggs> =>
    pipe(
      searchTransactions(txnTable)(ddb)({ committeeId: txn.committeeId }),
      taskEither.map(txnsToAgg(txn.committeeId)),
      taskEither.chain(putAggAndDecode(aggTable)(ddb))
    );

export const refreshAggs =
  (aggTable: string) =>
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (committeeId: string): TaskEither<ApplicationError, IAggs> =>
    pipe(
      searchTransactions(txnTable)(ddb)({ committeeId }),
      taskEither.map(txnsToAgg(committeeId)),
      taskEither.chain(putAggAndDecode(aggTable)(ddb))
    );
