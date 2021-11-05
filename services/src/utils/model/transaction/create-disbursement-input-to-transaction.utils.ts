import { ITransaction } from "../../../model/transaction.type";
import { genTxnId } from "../../gen-txn-id.utils";
import { Direction } from "../../enums/direction.enum";
import { now } from "../../time.utils";
import { Source } from "../../enums/source.enum";
import { TransactionType } from "../../enums/transaction-type.enum";
import { CreateDisbursementInput } from "../../../graphql/input-types/create-disbursement.input-type";

export const createDisbInputToTxn =
  (createdByUser: string) =>
  (d: CreateDisbursementInput): ITransaction => ({
    id: genTxnId(),
    direction: Direction.Out,
    bankVerified: false,
    ruleVerified: true,
    createdByUser,
    initiatedTimestamp: now(),
    source: Source.DASHBOARD,
    transactionType: TransactionType.Disbursement,
    ...d,
  });
