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
import { IStratoSDKConfig } from "../clients/dapp/dapp.decoders";
import {
  getCommitteeById,
  ICommittee,
} from "../queries/get-committee-by-id.query";
import { commitTransaction } from "../clients/dapp/dapp.client";
import { verifiedTxnToStrato } from "./verified-txn-to-strato.pipe";

const logPrefix = "Audit Logs";

export enum DdbEventName {
  MODIFY = "MODIFY",
  INSERT = "INSERT",
  REMOVE = "REMOVE",
}

export const auditTxnStream =
  (stratoConf: IStratoSDKConfig) =>
  (comTable: string) =>
  (txnsTable: string) =>
  (auditLogsTable: string) =>
  (ddb: DynamoDB) =>
  (ddbEvent: DdbEventName) =>
  (oldImg: unknown) =>
  (newImg: unknown): TaskEither<ApplicationError, ITxnAuditLog> =>
    pipe(
      taskEither.of(imgsToRaw([oldImg, newImg])),
      taskEither.chain(rawToTxns),
      taskEither.map(txnsToAuditLog(ddbEvent)),
      taskEither.chain(
        commitTxnToLedgerIfVerified(stratoConf)(comTable)(txnsTable)(ddb)
      ),
      taskEither.chain(putTxnAuditLogAndDecode(auditLogsTable)(ddb))
    );

const isVerified = (txn: ITransaction) => txn.bankVerified && txn.ruleVerified;

const commitTxnToLedgerIfVerified =
  (config: IStratoSDKConfig) =>
  (comTable: string) =>
  (txnsTableName: string) =>
  (ddb: DynamoDB) =>
  (auditLog: ITxnAuditLog): TaskEither<ApplicationError, ITxnAuditLog> => {
    console.log("checking verification for rules run");
    console.log("new txn is verified", isVerified(auditLog.newTransaction));
    console.log("old txn is verified", isVerified(auditLog.oldTransaction));
    if (
      isVerified(auditLog.newTransaction) &&
      !isVerified(auditLog.oldTransaction)
    ) {
      console.log("picked up a verified txn");
      return pipe(
        verifiedTxnToStrato(config)(txnsTableName)(comTable)(ddb)(
          auditLog.newTransaction
        ),
        taskEither.chain(() => taskEither.of(auditLog))
      );
    } else return taskEither.of(auditLog);
  };

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
