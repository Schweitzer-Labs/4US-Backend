import { IStratoSDKConfig } from "../clients/dapp/dapp.decoders";
import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../model/transaction.type";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { pipe } from "fp-ts/function";
import { getCommitteeById } from "../utils/model/committee/get-committee-by-id.query";
import { taskEither } from "fp-ts";
import { commitTransaction } from "../clients/dapp/dapp.client";

export const verifiedTxnToStrato =
  (stratoConf: IStratoSDKConfig) =>
  (txnTable: string) =>
  (comTable: string) =>
  (ddb: DynamoDB) =>
  (txn: ITransaction): TaskEither<ApplicationError, ITransaction> =>
    pipe(
      getCommitteeById(comTable)(ddb)(txn.committeeId),
      taskEither.chain((committee) =>
        commitTransaction(stratoConf)(txnTable)(ddb)(committee)(txn)
      )
    );
