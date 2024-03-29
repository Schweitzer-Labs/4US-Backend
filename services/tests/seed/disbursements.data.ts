import { ITransaction } from "../../src/model/transaction.type";
import { now } from "../../src/utils/time.utils";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import { Source } from "../../src/utils/enums/source.enum";

export const disbursementsData: ITransaction[] = [
  {
    id: genTxnId(),
    committeeId: "pat-miller",
    direction: "out",
    amount: 1000,
    paymentMethod: "ach",
    bankVerified: true,
    ruleVerified: false,
    initiatedTimestamp: now(),
    transactionType: "disbursement",
    source: Source.FINICITY,
  },
  {
    id: genTxnId(),
    committeeId: "pat-miller",
    direction: "out",
    amount: 12000,
    paymentMethod: "ach",
    bankVerified: true,
    ruleVerified: false,
    initiatedTimestamp: now(),
    transactionType: "disbursement",
    source: Source.FINICITY,
  },
  {
    id: genTxnId(),
    committeeId: "pat-miller",
    direction: "out",
    amount: 123000,
    paymentMethod: "ach",
    bankVerified: true,
    ruleVerified: false,
    initiatedTimestamp: now(),
    transactionType: "disbursement",
    source: Source.FINICITY,
  },
  {
    id: genTxnId(),
    committeeId: "pat-miller",
    direction: "out",
    amount: 49000,
    paymentMethod: "ach",
    bankVerified: true,
    ruleVerified: false,
    initiatedTimestamp: now(),
    transactionType: "disbursement",
    source: Source.FINICITY,
  },
  {
    id: genTxnId(),
    committeeId: "pat-miller",
    direction: "out",
    amount: 194000,
    paymentMethod: "ach",
    bankVerified: true,
    ruleVerified: false,
    initiatedTimestamp: now(),
    transactionType: "disbursement",
    source: Source.FINICITY,
  },
  {
    id: genTxnId(),
    committeeId: "pat-miller",
    direction: "out",
    amount: 1500,
    paymentMethod: "ach",
    bankVerified: true,
    ruleVerified: false,
    initiatedTimestamp: now(),
    transactionType: "disbursement",
    source: Source.FINICITY,
  },
];
