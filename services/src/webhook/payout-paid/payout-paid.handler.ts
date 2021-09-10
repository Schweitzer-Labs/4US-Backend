import { Stripe } from "stripe";
import { fold, TaskEither } from "fp-ts/TaskEither";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";
import { successResponse } from "../../utils/success-response";
import { midnightLastNight, milliToEpoch } from "../../utils/time.utils";
import { now } from "fp-ts/Date";
import { ApplicationError } from "../../utils/application-error";
import {
  decodePayoutPaidEvent,
  decodeRunReportRes,
  IRunReportRes,
} from "./payout-paid.decoder";

export const runReport =
  (stripe: Stripe) => async (connectAccountId: string) => {
    console.log("report run called with connect account id", connectAccountId);
    const res = await stripe.reporting.reportRuns.create({
      report_type: "connected_account_payout_reconciliation.itemized.5",
      parameters: {
        connected_account: connectAccountId,
        interval_start: milliToEpoch(now()) - 60 * 60 * 24 * 14,
        interval_end:
          milliToEpoch(midnightLastNight()) - 60 * 60 * 24 - 60 * 60 * 4,
        columns: [
          "automatic_payout_id",
          "automatic_payout_effective_at_utc",
          "gross",
          "fee",
          "net",
          "balance_transaction_id",
          "payment_intent_id",
          "connected_account",
        ],
      },
    });

    console.log("reportRuns res", JSON.stringify(res));

    return res;
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

export const handlePayoutPaid = (stripe: Stripe) => async (payload: unknown) =>
  await pipe(
    decodePayoutPaidEvent(payload),
    taskEither.map((res) => res.account),
    taskEither.chain(runReportAndDecode(stripe)),
    fold(
      (error) => task.of(error.toResponse()),
      () => task.of(successResponse)
    )
  )();
