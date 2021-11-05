import { DynamoDB } from "aws-sdk";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { IAggs } from "../model/aggs.type";
import { pipe } from "fp-ts/function";
import { searchTransactions } from "../utils/model/transaction/search-transactions.query";
import { taskEither } from "fp-ts";
import { txnsToAgg } from "../utils/model/aggs/txns-to-agg.utils";
import { putAggAndDecode } from "../utils/model/aggs/put-aggs.utils";
import { ITransaction } from "../model/transaction.type";

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
