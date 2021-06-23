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
import { ICommittee } from "./queries/get-committee-by-id.query";
import { ITransaction } from "./queries/search-transactions.decoder";
import { get_committee_by_stripe_account_and_decode } from "./utils/model/get-committee-by-stripe-account.utils";
import { get_bank_unverified_contributions_not_paid_out } from "./utils/model/get-bank-unverified-contributions.utils";

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

export const syncPayout =
  (txnsTableName: string) =>
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (stripeAccountId: string) =>
  (reportRows: IPayoutReportRows) =>
    pipe(
      get_committee_by_stripe_account_and_decode(committeeTableName)(dynamoDB)(
        stripeAccountId
      ),
      taskEither.chain(
        get_bank_unverified_contributions_not_paid_out(txnsTableName)(dynamoDB)
      ),
      taskEither.chain(matchReportRowsWithTxns(reportRows)),
      taskEither.chain(insertTxns(txnsTableName)(dynamoDB))
    );

export const matchReportRowsWithTxns =
  (reportRows: IPayoutReportRows) =>
  (txns: ITransaction[]): TaskEither<ApplicationError, ITransaction[]> => {};

export const insertTxns =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (txns: ITransaction[]): TaskEither<ApplicationError, ITransaction[]> => {};
