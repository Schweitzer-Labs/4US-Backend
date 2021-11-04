import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../application-error";
import { taskEither as te } from "fp-ts";
import { ITxnAuditLog } from "../../model/audit-log.type";

export const putTxnAuditLog =
  (auditLogsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (auditLog: ITxnAuditLog) => {
    const marshalledLog = DynamoDB.Converter.marshall(auditLog);
    await dynamoDB
      .putItem({
        TableName: auditLogsTableName,
        Item: marshalledLog,
      })
      .promise();
    return auditLog;
  };

export const putTxnAuditLogAndDecode =
  (auditLogsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (auditLog: ITxnAuditLog): TaskEither<ApplicationError, ITxnAuditLog> =>
    pipe(
      te.tryCatch(
        () => putTxnAuditLog(auditLogsTableName)(dynamoDB)(auditLog),
        (e) => new ApplicationError("Put txn audit log failed", e)
      )
    );
