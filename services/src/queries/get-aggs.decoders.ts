import * as t from "io-ts";

export const Aggs = t.type({
  balance: t.number,
  totalRaised: t.number,
  totalSpent: t.number,
  totalDonors: t.number,
  totalTransactions: t.number,
  totalContributionsInProcessing: t.number,
  totalDisbursementsInProcessing: t.number,
  needsReviewCount: t.number,
  committeeId: t.string,
});

export type IAggs = t.TypeOf<typeof Aggs>;
