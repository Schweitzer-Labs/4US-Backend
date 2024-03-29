import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { getTxnById } from "../utils/model/transaction/get-txn-by-id.utils";
import { taskEither as te } from "fp-ts";
import { putTransactionAndDecode } from "../utils/model/transaction/put-transaction.utils";
import { ITransaction } from "../model/transaction.type";
import { isRuleVerifiedBankUnverifiedContrib } from "../utils/model/transaction/is-non-verified-contrib.utils";
import { AmendContributionInput } from "../graphql/input-types/amend-contrib.input-type";

export const amendContrib =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (modifiedByUser: string) =>
  (committeeId: string) =>
  (txnId: string) =>
  (contribInput: AmendContributionInput) =>
    pipe(
      getTxnById(txnsTableName)(dynamoDB)(committeeId)(txnId),
      te.chain(isRuleVerifiedBankUnverifiedContrib),
      te.map(mergeContribInputWithTxn(modifiedByUser)(contribInput)),
      te.chain(putTransactionAndDecode(txnsTableName)(dynamoDB))
    );

const mergeContribInputWithTxn =
  (modifiedByUser: string) =>
  (amendContributionInput: AmendContributionInput) =>
  (txn: ITransaction): ITransaction => ({
    ...txn,
    ...amendContributionInput,
    modifiedByUser,
  });
