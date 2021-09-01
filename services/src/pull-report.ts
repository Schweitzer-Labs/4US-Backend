import { Stripe } from "stripe";
import { midnightLastNight, milliToEpoch } from "./utils/time.utils";
import { now } from "fp-ts/Date";
import { getReport } from "./webhook/run-report-succeeded/report-run-succeeded.handler";

const apiKey =
  "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";
const stripe = new Stripe(apiKey, {
  apiVersion: "2020-08-27",
});

const committeeAccount = "acct_1IQFp0RNcAftf9zR";

const createReportRun = async () => {
  const res = await stripe.reporting.reportRuns.create({
    report_type: "connected_account_payout_reconciliation.itemized.5",
    parameters: {
      connected_account: committeeAccount,
      interval_start: milliToEpoch(now()) - 60 * 60 * 24 * 90,
      interval_end: milliToEpoch(midnightLastNight()),
      columns: [
        "automatic_payout_id",
        "automatic_payout_effective_at_utc",
        "gross",
        "balance_transaction_id",
        "payment_intent_id",
        "charge_id",
        "connected_account",
      ],
    },
  });

  console.log(res);

  return res;
};

// createReportRun();

const requestReport = async () => {
  const res = await getReport(apiKey)(
    "https://files.stripe.com/v1/files/file_1JEzOmEUhH8cxK5gn4KKyrsq/contents"
  );

  console.log(res);

  return res;
};

requestReport();
