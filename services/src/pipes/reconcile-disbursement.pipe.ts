import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../queries/search-transactions.decoder";
import { VerifyDisbursementInput } from "../input-types/verify-disbursement.input-type";
import { pipe } from "fp-ts/function";
import { getTxnById } from "../utils/model/get-txn-by-id.utils";
import { taskEither as te } from "fp-ts";
import { isNonVerifiedDisbursement } from "../utils/model/is-non-verified-disbursement.utils";
import { validateDisbursement } from "../utils/model/validate-disbursement.utils";
import { putTransactionAndDecode } from "../utils/model/put-transaction.utils";
import { ReconcileDisbursementInput } from "../input-types/reconcile-disbursement.input-type";

export const reconcileDisbursement =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string) =>
  (txnId: string) =>
  (selectedTxnIds: string[]) =>
  (rd: ReconcileDisbursementInput) =>
    pipe(
      getTxnById(txnsTableName)(dynamoDB)(committeeId)(txnId),
      te.chain(isNonVerifiedDisbursement),
      te.map(mergeDisbursementInputWithTxn(disbursementInput)),
      te.chain(validateDisbursement),
      te.map(approveTxn),
      te.chain(putTransactionAndDecode(txnsTableName)(dynamoDB))
    );

const setTxnsAsReconciled = (txn: ITransaction) => txn;

const approveTxn = (txn: ITransaction): ITransaction => ({
  ...txn,
  ruleVerified: true,
});

const mergeDisbursementInputWithTxn =
  (disbursementInput: VerifyDisbursementInput) =>
  (txn: ITransaction): ITransaction => ({
    ...txn,
    ...disbursementInput,
  });
