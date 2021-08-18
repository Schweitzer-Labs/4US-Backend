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
import * as dotenv from "dotenv";

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
  district: "53",
  officeType: "senate",
  party: "democrat",
  race: "primary",
  ruleVersion: "nyboe-2020",
  scope: "state",
  state: "ny",
  stripeAccount,
  finicityCustomerId: "5007489410",
  finicityAccountId: "5016000964",
});

describe("Model Utils", function () {
  before(async () => {
    await putCommittee(committeesTableName)(dynamoDB)(committee);
    await sleep(1000);

    const lazyRes = await syncCommittee(config)(txnsTableName)(dynamoDB)(
      committee
    )();
    if (isLeft(lazyRes)) {
      throw new ApplicationError("sync failed", lazyRes.left);
    }
    const m = lazyRes.right;
    await Promise.all(m.map((f) => f()).map(async (f) => await f));
  });
  describe("get_committee_by_stripe_account_and_decode", function () {
    it("Retrieves a committee by stripe account id", async () => {
      const res = await get_committee_by_stripe_account_and_decode(
        committeesTableName
      )(dynamoDB)(stripeAccount)();
      if (isLeft(res)) {
        throw new ApplicationError("Committee look up failed", {});
      }
      expect(res.right.stripeAccount).to.equal(stripeAccount);
    });

    it("Fails when an non-existent Stripe ID is passed", async () => {
      const res = await get_committee_by_stripe_account_and_decode(
        committeesTableName
      )(dynamoDB)(genTxnId())();

      expect(isLeft(res)).to.equal(true);
    });
  });
  describe("get_bank_unverified_contributions_not_paid_out", function () {
    it("Retrieves unverified contributions not paid out to committee", async () => {
      const res = await get_bank_unverified_contributions_not_paid_out(
        txnsTableName
      )(dynamoDB)(committee)();
      if (isLeft(res)) {
        throw new ApplicationError("Transaction look up failed", {});
      }
      expect(res.right.length).to.equal(0);
    });
  });
  describe("update_txn_with_stripe_payout_id", function () {
    let testTxn: ITransaction;
    before(async () => {
      testTxn = genContributionRecord(committee.id);
      await putTransaction(txnsTableName)(dynamoDB)(testTxn);
      await sleep(1000);

      testTxn.stripePayoutId = genTxnId();
      testTxn.stripeAutomaticPayoutEffectiveAtUtc = 11111111111;
      await updateTxnWithStripePayoutId(txnsTableName)(dynamoDB)(testTxn);
      await sleep(1000);
    });
    it("Updates an existing transaction with a stripe payout id", async () => {
      const res = await getTxnById(txnsTableName)(dynamoDB)(committee.id)(
        testTxn.id
      )();

      if (isLeft(res)) {
        throw new ApplicationError("Transaction look up failed", {});
      }
      expect(res.right.stripePayoutId).to.equal(testTxn.stripePayoutId);
    });
    after(async () => {
      // await deleteTxn(txnsTableName)(dynamoDB)(testTxn);
    });
  });

  after(async () => {
    await deleteCommittee(committeesTableName)(dynamoDB)(committee);
  });
});
