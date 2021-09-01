import axios from "axios";
import { fold, TaskEither } from "fp-ts/TaskEither";
import { task, taskEither } from "fp-ts";
import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { ApplicationError } from "../../utils/application-error";
import { get_committee_by_stripe_account_and_decode } from "../../utils/model/get-committee-by-stripe-account.utils";
import { get_bank_unverified_contributions_not_paid_out } from "../../utils/model/get-bank-unverified-contributions.utils";
import { ITransaction } from "../../queries/search-transactions.decoder";
import { successResponse } from "../../utils/success-response";
import { update_txns_with_stripe_payout_id } from "../../utils/model/update-txns.utils";
import {
  decodePayoutReportRows,
  IPayoutReportRows,
  parseCSVAndDecode,
} from "./report-run-succeeded.decoder";
import {
  decodeReportRunEvent,
  IReportRunEvent,
  reportEventToStripeAccount,
  reportEventToUrl,
} from "../payout-paid/payout-paid.decoder";

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
      taskEither.map(matchReportRowsWithTxns(reportRows)),
      taskEither.chain(saveMatchedTxns(txnsTableName)(dynamoDB))
    );

export const matchReportRowsWithTxns =
  (reportRows: IPayoutReportRows) =>
  (txns: ITransaction[]): ITransaction[] =>
    txns.reduce((acc: ITransaction[], txn) => {
      const resList = reportRows.filter(
        (row) => txn.stripePaymentId === row.charge_id
      );
      if (resList.length === 1) {
        const [match] = resList;
        const matchedTxn: ITransaction = {
          ...txn,
          stripePayoutId: match.automatic_payout_id,
          stripeAutomaticPayoutEffectiveAtUtc: new Date(
            match.automatic_payout_effective_at_utc
          ).getTime(),
        };
        return [...acc, matchedTxn];
      } else if (resList.length > 1) {
        new ApplicationError(
          "Duplicate balance transactions found",
          JSON.stringify(resList)
        );
      }
      return acc;
    }, []);

export const saveMatchedTxns =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (txns: ITransaction[]): TaskEither<ApplicationError, ITransaction[]> =>
    pipe(
      taskEither.tryCatch(
        () => update_txns_with_stripe_payout_id(txnsTableName)(dynamoDB)(txns),
        (error) =>
          new ApplicationError(
            "update_txns_with_stripe_payout_id failed",
            error
          )
      )
    );

export const decodeCSVAndSyncPayouts =
  (txnsTableName: string) =>
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (stripeAccount: string) =>
  (csv: string) =>
    pipe(
      parseCSVAndDecode(csv),
      taskEither.chain(decodePayoutReportRows),
      taskEither.chain(
        syncPayout(txnsTableName)(committeeTableName)(dynamoDB)(stripeAccount)
      )
    );

export const handleReportRunSucceeded =
  (txnsTableName: string) =>
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (stripeApiKey: string) =>
  async (payload: unknown) =>
    await pipe(
      decodeReportRunEvent(payload),
      taskEither.chain((reportEvent) =>
        pipe(
          taskEither.of(reportEventToUrl(reportEvent)),
          taskEither.chain(getReportAndDecode(stripeApiKey)),
          taskEither.chain(
            decodeCSVAndSyncPayouts(txnsTableName)(committeeTableName)(
              dynamoDB
            )(reportEventToStripeAccount(reportEvent))
          )
        )
      ),
      fold(
        (error) => task.of(error.toResponse()),
        () => task.of(successResponse)
      )
    )();
