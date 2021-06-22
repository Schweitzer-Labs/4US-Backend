import { Stripe } from "stripe";
import { midnightLastNight, milliToEpoch, now } from "./utils/time.utils";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "./utils/application-error";
import axios from "axios";
import { taskEither } from "fp-ts";
import {
  decodeRunReportRes,
  IPayoutReportRows,
  IReportRunEvent,
  IRunReportRes,
  parseCSV,
} from "./webhook/webhook.decoders";
import { pipe } from "fp-ts/function";
import { DynamoDB } from "aws-sdk";
import { getCommitteeByStripeAccountAndDecode } from "./pipes/sync-payout.pipe";
import { ICommittee } from "./queries/get-committee-by-id.query";
import { ITransaction } from "./queries/search-transactions.decoder";

export const runReport =
  (stripe: Stripe) => async (connectAccountId: string) => {
    return await stripe.reporting.reportRuns.create({
      report_type: "connected_account_payout_reconciliation.itemized.5",
      parameters: {
        connected_account: connectAccountId,
        interval_start: milliToEpoch(now()) - 60 * 60 * 24 * 14,
        interval_end:
          milliToEpoch(midnightLastNight()) - 60 * 60 * 24 - 60 * 60 * 4,
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

export const matchPayoutReportRowsToTxns =
  (txnsTableName: string) =>
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (stripeAccountId: string) =>
  (reportRows: IPayoutReportRows) =>
    pipe(
      getCommitteeByStripeAccountAndDecode(committeeTableName)(dynamoDB)(
        stripeAccountId
      ),
      taskEither.chain(
        get_bank_unverified_contributions_not_paid_out_by_committee_and_decode(
          txnsTableName
        )(dynamoDB)
      ),
      taskEither.chain(match_report_row_with_txn(reportRows)),
      taskEither.chain(updateTxns)
    );

export const get_bank_unverified_contributions_not_paid_out_by_committee_and_decode =

    (txnsTable: string) =>
    (dynamoDB: DynamoDB) =>
    (committee: ICommittee): TaskEither<ApplicationError, ITransaction> => {};

export const match_report_row_with_txn =
  (reportRows: IPayoutReportRows) =>
  (txns: ITransaction): TaskEither<ApplicationError, ITransaction[]> => {};
