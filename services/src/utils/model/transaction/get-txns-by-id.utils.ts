import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../../model/transaction.type";
import { getTxnById } from "./get-txn-by-id.utils";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../application-error";
import { taskEither } from "fp-ts";
import * as Array from "fp-ts/Array";

export const getTxnsById =
  (txnsTableName: string) =>
  (ddb: DynamoDB) =>
  (committeeId: string) =>
  (txnIds: string[]): TaskEither<ApplicationError, ITransaction[]> =>
    Array.traverse(taskEither.ApplicativeSeq)(
      getTxnById(txnsTableName)(ddb)(committeeId)
    )(txnIds);
