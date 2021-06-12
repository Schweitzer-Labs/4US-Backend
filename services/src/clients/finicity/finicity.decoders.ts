import * as t from "io-ts";

export const FinicityConfig = t.type({
  partnerId: t.string,
  partnerSecret: t.string,
  appKey: t.string,
});

export type FinicityConfig = t.TypeOf<typeof FinicityConfig>;

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
  type: t.string,
  checkNum: t.string,
});

export const FinicityTransaction = t.intersection([
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

export type IFinicityTransaction = t.TypeOf<typeof FinicityTransaction>;
