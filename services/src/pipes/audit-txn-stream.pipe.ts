import { DynamoDB } from "aws-sdk";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { ITxnAuditLog } from "../queries/get-audit-logs.decoders";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { putTxnAuditLogAndDecode } from "../utils/model/put-txn-audit-log.utils";
import * as AWS from "aws-sdk";
import {
  currentVersion,
  ITransaction,
  Transaction,
} from "../queries/search-transactions.decoder";
import { validateDDBResponse } from "../repositories/ddb.utils";
import { now } from "../utils/time.utils";

const logPrefix = "Audit Logs";

export enum DdbEventName {
  MODIFY = "MODIFY",
  INSERT = "INSERT",
  REMOVE = "REMOVE",
}

export const auditTxnStream =
  (auditLogsTable: string) =>
  (ddb: DynamoDB) =>
  (ddbEvent: DdbEventName) =>
  (oldImg: unknown) =>
  (newImg: unknown): TaskEither<ApplicationError, ITxnAuditLog> =>
    pipe(
      taskEither.of(imgsToRaw([oldImg, newImg])),
      taskEither.chain(rawToTxns),
      taskEither.map(txnsToAuditLog(ddbEvent)),
      taskEither.chain(putTxnAuditLogAndDecode(auditLogsTable)(ddb))
    );

const imgsToRaw = ([oldImg, newImg]: [any, any]): [unknown, unknown] => [
  AWS.DynamoDB.Converter.unmarshall(oldImg),
  AWS.DynamoDB.Converter.unmarshall(newImg),
];

const rawToTxns = ([oldImg, newImg]: [unknown, unknown]): TaskEither<
  ApplicationError,
  [ITransaction, ITransaction]
> =>
  pipe(
    validateDDBResponse(logPrefix)(Transaction)(oldImg),
    taskEither.chain((oldTxn) =>
      pipe(
        validateDDBResponse(logPrefix)(Transaction)(newImg),
        taskEither.chain((newTxn) => taskEither.of([oldTxn, newTxn]))
      )
    )
  );

const txnsToAuditLog =
  (ddbEvent: DdbEventName) =>
  ([oldTransaction, newTransaction]: [
    ITransaction,
    ITransaction
  ]): ITxnAuditLog => ({
    committeeId: newTransaction.committeeId,
    userId: newTransaction.modifiedByUser,
    id: newTransaction.id,
    newTransaction,
    oldTransaction,
    version: currentVersion,
    type: "Transaction",
    ddbEvent,
    timestamp: now(),
  });
