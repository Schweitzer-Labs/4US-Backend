import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { getTxnById } from "../utils/model/get-txn-by-id.utils";
import { taskEither as te } from "fp-ts";
import { validateDisbursement } from "../utils/model/validate-disbursement.utils";
import { putTransactionAndDecode } from "../utils/model/put-transaction.utils";
import { ITransaction } from "../queries/search-transactions.decoder";
import { AmendDisbInput } from "../input-types/amend-disb.input-type";
import {
  isNonBankVerifiedDisb,
  isNotABankVerifiedRuleUnverifiedDisb,
} from "../utils/model/is-non-verified-disbursement.utils";

export const amendDisb =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string) =>
  (txnId: string) =>
  (disbursementInput: AmendDisbInput) =>
    pipe(
      getTxnById(txnsTableName)(dynamoDB)(committeeId)(txnId),
      te.chain(isNotABankVerifiedRuleUnverifiedDisb),
      te.map(mergeDisbInputWithTxn(disbursementInput)),
      te.chain(validateDisbursement),
      te.chain(putTransactionAndDecode(txnsTableName)(dynamoDB))
    );

const mergeDisbInputWithTxn =
  (disbursementInput: AmendDisbInput) =>
  (txn: ITransaction): ITransaction => ({
    ...txn,
    ...disbursementInput,
  });
