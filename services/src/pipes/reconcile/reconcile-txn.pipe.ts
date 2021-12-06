import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../model/transaction.type";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";
import { ICommittee } from "../../model/committee.type";
import { ReconcileTxnInput } from "../../graphql/input-types/reconcile-txn.input-type";
import { recExtContrib } from "./rec-ext-contrib.pipe";
import { recPlatform } from "./rec-platform.pipe";
import { ILoadedRecInput, loadRecInput } from "./rec-utils.pipe";

const firstTxnIsExternalContrib = (txns: ITransaction[]): boolean =>
  txns.length > 0 ? !!txns[0].externalTransactionId : false;

const loadedRecInputIsExtContrib = ({
  selectedTxns,
}: ILoadedRecInput): boolean => firstTxnIsExternalContrib(selectedTxns);

export const recTxns =
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (recInput: ReconcileTxnInput) =>
  (com: ICommittee): TaskEither<ApplicationError, ITransaction> =>
    pipe(
      loadRecInput(txnTable)(ddb)(recInput)(com),
      taskEither.chain((input) =>
        pipe(
          loadedRecInputIsExtContrib(input)
            ? recExtContrib(txnTable)(ddb)(input)
            : recPlatform(txnTable)(ddb)(input)
        )
      )
    );
