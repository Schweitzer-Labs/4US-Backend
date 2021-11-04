import { DynamoDB } from "aws-sdk";
import { ITransaction, Transactions } from "../../types/transaction.type";
import { requestTxnById } from "./get-txn-by-id.utils";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../application-error";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { validateDDBResponse } from "../ddb.utils";

const logPrefix = "Get Txns by Id";

export const getTxnsByIdRequest =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string) =>
  async (txnIds: string[]): Promise<unknown> => {
    const txns = [];
    for (const txnId of txnIds) {
      const txn = await requestTxnById(txnsTableName)(dynamoDB)(committeeId)(
        txnId
      );
      txns.push(txn);
    }

    return txns;
  };

export const getTxnsById =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string) =>
  (txnIds: string[]): TaskEither<ApplicationError, ITransaction[]> =>
    pipe(
      taskEither.tryCatch(
        () => getTxnsByIdRequest(txnsTableName)(dynamoDB)(committeeId)(txnIds),
        (e) => new ApplicationError("get txns by id failed", e)
      ),
      taskEither.chain(validateDDBResponse(logPrefix)(Transactions))
    );
