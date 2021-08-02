import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { getTxnById } from "../utils/model/get-txn-by-id.utils";
import { taskEither as te } from "fp-ts";
import { validateDisbursement } from "../utils/model/validate-disbursement.utils";
import { putTransactionAndDecode } from "../utils/model/put-transaction.utils";
import { ITransaction } from "../queries/search-transactions.decoder";
import { AmendDisbInput } from "../input-types/amend-disb.input-type";
import { isNotABankVerifiedRuleUnverifiedDisb } from "../utils/model/is-non-verified-disbursement.utils";

export const amendDisb =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (modifiedByUser: string) =>
  (committeeId: string) =>
  (txnId: string) =>
  (disbursementInput: AmendDisbInput) =>
    pipe(
      getTxnById(txnsTableName)(dynamoDB)(committeeId)(txnId),
      te.chain(isNotABankVerifiedRuleUnverifiedDisb),
      te.map(mergeDisbInputWithTxn(modifiedByUser)(disbursementInput)),
      te.chain(validateDisbursement),
      te.chain(putTransactionAndDecode(txnsTableName)(dynamoDB))
    );

const mergeDisbInputWithTxn =
  (modifiedByUser: string) =>
  (disbursementInput: AmendDisbInput) =>
  (txn: ITransaction): ITransaction => ({
    ...txn,
    ...disbursementInput,
    modifiedByUser,
  });
