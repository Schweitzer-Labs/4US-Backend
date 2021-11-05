import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { getTxnById } from "../utils/model/transaction/get-txn-by-id.utils";
import { taskEither as te } from "fp-ts";
import { validateDisbursement } from "../utils/model/transaction/validate-disbursement.utils";
import { putTransactionAndDecode } from "../utils/model/transaction/put-transaction.utils";
import { ITransaction } from "../model/transaction.type";
import { AmendDisbInput } from "../graphql/input-types/amend-disb.input-type";
import { isNotABankVerifiedRuleUnverifiedDisb } from "../utils/model/transaction/is-non-verified-disbursement.utils";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";

export const amendDisb =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (modifiedByUser: string) =>
  (committeeId: string) =>
  (txnId: string) =>
  (
    disbursementInput: AmendDisbInput
  ): TaskEither<ApplicationError, ITransaction> =>
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
