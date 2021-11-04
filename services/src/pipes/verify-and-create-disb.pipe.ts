import { DynamoDB } from "aws-sdk";
import { ILexisNexisConfig } from "../clients/lexis-nexis/lexis-nexis.client";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { ITransaction } from "../types/transaction.type";
import { pipe } from "fp-ts/function";
import { createDisbInputToTxn } from "../utils/model/create-disbursement-input-to-transaction.utils";
import { taskEither as te } from "fp-ts";
import { runBIDonTxn } from "../clients/lexis-nexis/business-id.request";
import { putTransactionAndDecode } from "../utils/model/put-transaction.utils";
import { CreateDisbursementInput } from "../graphql/input-types/create-disbursement.input-type";
import { ICommittee } from "../types/committee.type";

export const verifyAndCreateDisb =
  (user: string) =>
  (txnTable: string) =>
  (billTable: string) =>
  (ddb: DynamoDB) =>
  (config: ILexisNexisConfig) =>
  (committee: ICommittee) => {
    return (
      d: CreateDisbursementInput
    ): TaskEither<ApplicationError, ITransaction> =>
      pipe(
        te.of(createDisbInputToTxn(user)(d)),
        te.chain(runBIDonTxn(billTable)(ddb)(config)(committee)),
        te.chain(putTransactionAndDecode(txnTable)(ddb))
      );
  };
