import { expect } from "chai";
import { genCommittee } from "../utils/gen-committee.util";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { unverifiedContributionsData } from "../seed/unverified-contributions.data";
import { putTransaction } from "../../src/utils/model/put-transaction.utils";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import { putCommittee } from "../../src/utils/model/put-committee.utils";
import { sleep } from "../../src/utils/sleep.utils";
import { syncCommittee } from "../../src/pipes/finicity-bank-sync.pipe";
import { isLeft } from "fp-ts/Either";
import { ApplicationError } from "../../src/utils/application-error";
import { reconcileCommitteeContributions } from "../../src/pipes/reconcile-contributions.pipe";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const config = {
  partnerId: process.env.FINICITY_PARTNER_ID,
  partnerSecret: process.env.FINICITY_PARTNER_SECRET,
  appKey: process.env.FINICITY_APP_KEY,
};

const committee = genCommittee({
  district: "53",
  officeType: "senate",
  party: "democrat",
  race: "primary",
  ruleVersion: "nyboe-2020",
  scope: "state",
  state: "ny",
  tzDatabaseName: "America/New_York",
  finicityCustomerId: "5007489410",
  finicityAccountId: "5016000964",
});

describe("Reconciles a committees contributions", function () {
  before(async () => {
    // // Create committee
    // await putCommittee(committeesTableName)(dynamoDB)(committee);
    // await sleep(1000);
    //
    // // Sync transactions from finicity
    // const lazySyncRes = await syncCommittee(config)(txnsTableName)(dynamoDB)(
    //   committee
    // )();
    // if (isLeft(lazySyncRes)) {
    //   throw new ApplicationError("sync failed", lazySyncRes.left);
    // }
    // const m = lazySyncRes.right;
    // await Promise.all(m.map((f) => f()).map(async (f) => await f));
    //
    // await sleep(1000);
    //
    // // Import unverified contributions
    // for (const txn of unverifiedContributionsData) {
    //   txn.committeeId = committee.id;
    //   txn.id = genTxnId();
    //   await putTransaction(txnsTableName)(dynamoDB)(txn);
    // }
    //
    // await sleep(1000);

    committee.id = "1623932810440-SXSmGW";

    const syncRes = await reconcileCommitteeContributions(txnsTableName)(
      dynamoDB
    )(committee)();
    if (isLeft(syncRes)) {
      throw syncRes.left;
    }

    console.log(syncRes.right);

    // Run reconciliation
  });
  it("Sets contributions for associated payout to bank verified", async () => {
    console.log(committee.id);
    expect(false).to.equal(true);
  });
  it("Deletes payout transaction", async () => {
    console.log(committee.id);
    expect(false).to.equal(true);
  });
});
