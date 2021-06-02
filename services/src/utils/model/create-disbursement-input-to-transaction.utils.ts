import { CreateDisbursementInput } from "../../input-types/create-disbursement.input-type";
import { ITransaction } from "../../queries/search-transactions.decoder";
import { genTxnId } from "../gen-txn-id.utils";
import { Direction } from "../enums/direction.enum";
import { now } from "../time.utils";
import { Source } from "../enums/source.enum";

export const createDisbursementInputToTransaction =
  (createdByUser: string) =>
  (d: CreateDisbursementInput): ITransaction => ({
    id: genTxnId(),
    direction: Direction.OUT,
    bankVerified: false,
    ruleVerified: true,
    createdByUser,
    initiatedTimestamp: now(),
    source: Source.DASHBOARD,
    ...d,
  });
