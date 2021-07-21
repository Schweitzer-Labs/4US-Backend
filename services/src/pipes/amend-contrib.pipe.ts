import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { getTxnById } from "../utils/model/get-txn-by-id.utils";
import { taskEither as te } from "fp-ts";
import { putTransactionAndDecode } from "../utils/model/put-transaction.utils";
import { ITransaction } from "../queries/search-transactions.decoder";
import { AmendDisbInput } from "../input-types/amend-disb.input-type";
import { isRuleVerifiedBankUnverifiedContrib } from "../utils/model/is-non-verified-contrib.utils";
import { AmendContributionInput } from "../input-types/amend-contrib.input-type";

export const amendContrib =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string) =>
  (txnId: string) =>
  (contribInput: AmendContributionInput) =>
    pipe(
      getTxnById(txnsTableName)(dynamoDB)(committeeId)(txnId),
      te.chain(isRuleVerifiedBankUnverifiedContrib),
      te.map(mergeContribInputWithTxn(contribInput)),
      te.chain(putTransactionAndDecode(txnsTableName)(dynamoDB))
    );

const mergeContribInputWithTxn =
  (disbursementInput: AmendDisbInput) =>
  (txn: ITransaction): ITransaction => ({
    ...txn,
    ...disbursementInput,
  });
