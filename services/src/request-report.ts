import { Stripe } from "stripe";
import { midnightLastNight, milliToEpoch, now } from "./utils/time.utils";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "./utils/application-error";
import axios from "axios";
import { taskEither } from "fp-ts";
import {
  decodeRunReportRes,
  IReportRunEvent,
  IRunReportRes,
  parseCSV,
} from "./webhook/webhook.decoders";
import { pipe } from "fp-ts/function";

export const runReport =
  (stripe: Stripe) => async (connectAccountId: string) => {
    return await stripe.reporting.reportRuns.create({
      report_type: "connected_account_payout_reconciliation.itemized.5",
      parameters: {
        connected_account: connectAccountId,
        interval_start: milliToEpoch(now()) - 60 * 60 * 24 * 14,
        interval_end: milliToEpoch(midnightLastNight()) - 60 * 60 * 4,
        columns: [
          "automatic_payout_id",
          "automatic_payout_effective_at_utc",
          "balance_transaction_id",
          "payment_intent_id",
          "connected_account",
        ],
      },
    });
  };

export const runReportAndDecode =
  (stripe: Stripe) =>
  (connectAccountId: string): TaskEither<ApplicationError, IRunReportRes> =>
    pipe(
      taskEither.tryCatch(
        () => runReport(stripe)(connectAccountId),
        (err) => new ApplicationError("Get report file request failed", err)
      ),
      taskEither.chain(decodeRunReportRes)
    );

export const reportEventToUrl = (event: IReportRunEvent): string =>
  event.data.object.result.url;

export const getReport =
  (stripeApiKey: string) =>
  async (url: string): Promise<string> => {
    const params = {
      headers: {
        Authorization: `Bearer ${stripeApiKey}`,
      },
    };
    const { data } = await axios.get(url, params);
    return data;
  };

export const getReportAndDecode =
  (stripeApiKey: string) =>
  (url: string): TaskEither<ApplicationError, string> =>
    taskEither.tryCatch(
      () => getReport(stripeApiKey)(url),
      (err) => new ApplicationError("Get report file request failed", err)
    );
