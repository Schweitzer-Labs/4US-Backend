import { expect } from "chai";

import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { isLeft } from "fp-ts/Either";
import { ApplicationError } from "../../src/utils/application-error";
import { get_committee_by_stripe_account_and_decode } from "../../src/utils/model/get-committee-by-stripe-account.utils";
import { putCommittee } from "../../src/utils/model/put-committee.utils";
import { sleep } from "../../src/utils/sleep.utils";
import { genCommittee } from "../utils/gen-committee.util";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import { get_bank_unverified_contributions_not_paid_out } from "../../src/utils/model/get-bank-unverified-contributions.utils";
import { deleteCommittee } from "../../src/utils/model/delete-committee.utils";
import { syncCommittee } from "../../src/pipes/finicity-bank-sync.pipe";
import { updateTxnWithStripePayoutId } from "../../src/utils/model/update-txns.utils";
import { genContributionRecord } from "../utils/gen-contribution.util";
import { putTransaction } from "../../src/utils/model/put-transaction.utils";
import { ITransaction } from "../../src/queries/search-transactions.decoder";
import { getTxnById } from "../../src/utils/model/get-txn-by-id.utils";
import { deleteTxn } from "../../src/utils/model/delete-txn.utils";
import * as dotenv from "dotenv";
import { unverifiedContributionsData } from "../seed/unverified-contributions.data";
import {
  decodeCSVAndSyncPayouts,
  syncPayout,
} from "../../src/webhook/run-report-succeeded/report-run-succeeded.handler";
import { payoutReconcilationReportData } from "../seed/payout-reconcilation-report.data";
import { searchTransactions } from "../../src/queries/search-transactions.query";
import { TransactionType } from "../../src/utils/enums/transaction-type.enum";
import { groupTxnsByPayout } from "../../src/utils/model/group-txns-by-payout.utils";
import { ICommittee } from "../../src/queries/get-committee-by-id.query";
import { isPayout } from "../../src/pipes/reconcile-contributions.pipe";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const dynamoDB = new DynamoDB();

const runenv: any = process.env.RUNENV;
const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;

const config = {
  partnerId: process.env.FINICITY_PARTNER_ID,
  partnerSecret: process.env.FINICITY_PARTNER_SECRET,
  appKey: process.env.FINICITY_APP_KEY,
};

const stripeAccount = genTxnId();

const committee = genCommittee({
  stripeAccount,
  finicityCustomerId: "5007489410",
  finicityAccountId: "5016000964",
  candidateLastName: "Schweitzer",
  candidateFirstName: "Will",
  tzDatabaseName: "America/New_York",
  state: "ny",
  scope: "local",
  party: "republican",
  race: "general",
  district: "",
  county: "saratoga",
  officeType: "supervisor",
  ruleVersion: "nyboe-2020",
});

const disableFinicity = ({
  finicityCustomerId,
  finicityAccountId,
  ...rest
}: ICommittee) => rest;

describe("Model Utils", function () {
  before(async () => {
    await putCommittee(committeesTableName)(dynamoDB)(committee);

    // Add contribution data
    for (const txn of unverifiedContributionsData) {
      txn.committeeId = committee.id;
      txn.id = genTxnId();
      txn.paymentDate = txn.initiatedTimestamp;
      await putTransaction(txnsTableName)(dynamoDB)(txn);
    }

    await sleep(1000);

    // Sync finicity data
    const lazyRes = await syncCommittee(config)(txnsTableName)(dynamoDB)(
      committee
    )();
    if (isLeft(lazyRes)) {
      throw new ApplicationError("sync failed", lazyRes.left);
    }
    const m = lazyRes.right;
    await Promise.all(m.map((f) => f()).map(async (f) => await f));

    await sleep(1000);

    // Sync payout data
    const syncRes = await decodeCSVAndSyncPayouts(txnsTableName)(
      committeesTableName
    )(dynamoDB)(stripeAccount)(payoutReconcilationReportData)();
    if (isLeft(syncRes)) {
      throw new ApplicationError("Payout sync failed", syncRes.left);
    }
    await sleep(2000);

    await putCommittee(committeesTableName)(dynamoDB)(
      disableFinicity(committee)
    );

    await sleep(2000);
  });

  it("Syncs transactions with a payout", async () => {
    const txnsRes = await searchTransactions(txnsTableName)(dynamoDB)({
      committeeId: committee.id,
      transactionType: TransactionType.Contribution,
    })();

    if (isLeft(txnsRes)) {
      throw new ApplicationError("Txns search failed", txnsRes.left);
    }
    const txns = txnsRes.right;

    const matches = txns.filter((txn) => !!txn.stripePayoutId);

    const bankPayouts = txns.filter(
      (txn) => txn.bankVerified && !txn.ruleVerified && isPayout(txn)
    );

    console.log(
      "Bank payouts",
      bankPayouts.map(({ amount }) => amount)
    );

    const payoutGroups = groupTxnsByPayout(matches);

    await runRec(txnsTableName)(dynamoDB)(payoutGroups)(bankPayouts);

    console.log(
      "group res",
      payoutGroups.map((obj) => ({
        ...obj,
        txns: obj.txns.map((txn) => txn.amount),
      }))
    );
    console.log(committee.id);

    // expect(txns.length).to.equal(matches.length);
  });

  // after(async () => {
  //   await deleteCommittee(committeesTableName)(dynamoDB)(committee);
  // });
});

const withinADayOf = (d1) => (d2) => Math.abs(d1 - d2) < 1000 * 60 * 60 * 24;
const runRec =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (payoutGroups) =>
  async (bankPayouts) => {
    let reconCount = 0;
    for (const txn of bankPayouts) {
      const matches = payoutGroups.filter((payout) => {
        return (
          txn.amount === payout.amount &&
          withinADayOf(txn.paymentDate)(payout.payoutDate)
        );
      });
      if (matches.length === 1) {
        const match = matches[0];
        console.log("match found");
        console.log("bank txn", txn);
        await deleteTxn(txnsTableName)(dynamoDB)(txn);
        for (const pTxn of match.txns) {
          console.log("setting txns to verified");
          await putTransaction(txnsTableName)(dynamoDB)({
            ...pTxn,
            finicityTransactionData: txn.finicityTransactionData,
            finicityTransactionId: txn.finicityTransactionId,
            finicityCategory: txn.finicityCategory,
            finicityBestRepresentation: txn.finicityBestRepresentation,
            finicityPostedDate: txn.finicityPostedDate,
            finicityTransactionDate: txn.finicityTransactionDate,
            finicityNormalizedPayeeName: txn.finicityNormalizedPayeeName,
            finicityDescription: txn.finicityDescription,
            bankVerified: true,
          });
          reconCount++;
        }
      }
    }
    console.log("rec count", reconCount);
  };
