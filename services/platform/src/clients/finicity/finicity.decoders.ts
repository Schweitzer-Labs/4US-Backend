import * as t from "io-ts";
import * as D from "io-ts/Decoder";

export const FinicityConfig = D.struct({
  partnerId: D.string,
  partnerSecret: D.string,
  appKey: D.string,
});

export type FinicityConfig = D.TypeOf<typeof FinicityConfig>;

const FinicityTransactionRequired = t.type({
  id: t.number,
  amount: t.number,
  accountId: t.number,
  status: t.string,
  description: t.string,
  postedDate: t.number,
  transactionDate: t.number,
  createdDate: t.number,
  categorization: t.type({
    normalizedPayeeName: t.string,
    category: t.string,
    bestRepresentation: t.string,
    country: t.string,
  }),
});

const FinicityTransactionOptional = t.partial({
  lastUpdatedDate: t.number,
});

const FinicityTransaction = t.intersection([
  FinicityTransactionRequired,
  FinicityTransactionOptional,
]);

const GetFinicityTransactionsResponseRequired = t.type({
  transactions: t.array(FinicityTransaction),
});

const GetFinicityTransactionsResponseOptional = t.partial({
  found: t.number,
  displaying: t.number,
  moreAvailable: t.string,
  fromDate: t.string,
  toDate: t.string,
  sort: t.string,
});

export const GetFinicityTransactionsResponse = t.intersection([
  GetFinicityTransactionsResponseRequired,
  GetFinicityTransactionsResponseOptional,
]);

export type FinicityTransaction = D.TypeOf<typeof FinicityTransaction>;
