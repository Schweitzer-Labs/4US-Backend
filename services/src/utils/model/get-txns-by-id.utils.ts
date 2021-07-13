import { DynamoDB } from "aws-sdk";
import {
  ITransaction,
  Transactions,
} from "../../queries/search-transactions.decoder";
import { requestTxnById } from "./get-txn-by-id.utils";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../application-error";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { validateDDBResponse } from "../../repositories/ddb.utils";

const logPrefix = "Get Txns by Id";

export const getTxnsByIdRequest =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (bankTxn: ITransaction) =>
  async (txnIds: string[]): Promise<unknown> => {
    const txns = [];
    for (const txnId of txnIds) {
      const txn = await requestTxnById(txnsTableName)(dynamoDB)(bankTxn.id)(
        txnId
      );
      txns.push(txn);
    }

    return txns;
  };

export const getTxnsById =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (bankTxn: ITransaction) =>
  (txnIds: string[]): TaskEither<ApplicationError, ITransaction[]> =>
    pipe(
      taskEither.tryCatch(
        () => getTxnsByIdRequest(txnsTableName)(dynamoDB)(bankTxn)(txnIds),
        (e) => new ApplicationError("get txns by id failed", e)
      ),
      taskEither.chain(validateDDBResponse(logPrefix)(Transactions))
    );
