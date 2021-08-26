import { IStratoSDKConfig } from "../clients/dapp/dapp.decoders";
import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../queries/search-transactions.decoder";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { pipe } from "fp-ts/function";
import { getCommitteeById } from "../queries/get-committee-by-id.query";
import { taskEither } from "fp-ts";
import { commitTransaction } from "../clients/dapp/dapp.client";

export const verifiedTxnToStrato =
  (stratoConf: IStratoSDKConfig) =>
  (txnTable: string) =>
  (comTable: string) =>
  (ddb: DynamoDB) =>
  (comId: string) =>
  (txn: ITransaction): TaskEither<ApplicationError, ITransaction> =>
    pipe(
      getCommitteeById(comTable)(ddb)(comId),
      taskEither.chain((committee) =>
        commitTransaction(stratoConf)(txnTable)(ddb)(committee)(txn)
      )
    );
