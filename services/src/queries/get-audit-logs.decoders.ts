import * as t from "io-ts";
import { Transaction } from "./search-transactions.decoder";

const TxnAuditLogRequired = t.type({
  committeeId: t.string,
  id: t.string,
  version: t.string,
  type: t.string,
  ddbEvent: t.string,
  timestamp: t.number,
  newTransaction: Transaction,
  modifiedByUser: t.string,
});

const TxnAuditLogOptional = t.type({
  oldTransaction: Transaction,
});

export const TxnAuditLog = t.intersection([
  TxnAuditLogRequired,
  TxnAuditLogOptional,
]);

export type ITxnAuditLog = t.TypeOf<typeof TxnAuditLog>;
