import { expect } from "chai";

import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { isLeft } from "fp-ts/Either";
import { ApplicationError } from "../../src/utils/application-error";
import { putCommittee } from "../../src/utils/model/committee/put-committee.utils";
import { sleep } from "../../src/utils/sleep.utils";
import { dateToTxnId, genTxnId } from "../../src/utils/gen-txn-id.utils";
import { syncCommittee } from "../../src/pipes/finicity-bank-sync.pipe";
import { putTransaction } from "../../src/utils/model/transaction/put-transaction.utils";

import { deleteTxn } from "../../src/utils/model/transaction/delete-txn.utils";
import * as dotenv from "dotenv";
import { decodeCSVAndSyncPayouts } from "../../src/webhook/run-report-succeeded/report-run-succeeded.handler";
import { searchTransactions } from "../../src/utils/model/transaction/search-transactions.query";
import { TransactionType } from "../../src/utils/enums/transaction-type.enum";
import { groupTxnsByPayout } from "../../src/utils/model/transaction/group-txns-by-payout.utils";
import { isPayout } from "../../src/pipes/reconcile-contributions.pipe";
import { launchCommittee } from "../../src/clients/dapp/dapp.client";
import {
  initStratoConfig,
  IStratoSDKConfig,
} from "../../src/clients/dapp/dapp.decoders";
import {
  getStratoENodeUrl,
  getStratoNodeUrl,
  getStratoOAuthClientId,
  getStratoOauthClientSecret,
  getStratoOAuthOpenIdDiscoveryUrl,
} from "../../src/utils/config";
import { disableFinicity } from "../../src/utils/disable-finicity.utils";
import { genCommittee } from "../utils/gen-committee.util";
import { unverifiedContributionsData } from "../seed/unverified-contributions.data";
import { payoutReconcilationReportData } from "../seed/payout-reconcilation-report.data";
import { deleteCommittee } from "../../src/utils/model/committee/delete-committee.utils";

dotenv.config();

const stripeAccount = genTxnId();

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

let nodeUrl: string;
let eNodeUrl: string;
let oauthClientId: string;
let oauthClientSecret: string;
let oauthOpenIdDiscoveryUrl: string;
let stratoConf: IStratoSDKConfig;

const ps = new AWS.SSM();

let committee = genCommittee({
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
  efsFilerId: 161,
});

describe("Model Utils", function () {
  before(async () => {
    if (
      !nodeUrl ||
      !eNodeUrl ||
      !oauthClientId ||
      !oauthClientSecret ||
      !oauthOpenIdDiscoveryUrl
    ) {
      nodeUrl = await getStratoNodeUrl(ps)(runenv);
      eNodeUrl = await getStratoENodeUrl(ps)(runenv);
      oauthClientId = await getStratoOAuthClientId(ps)(runenv);
      oauthClientSecret = await getStratoOauthClientSecret(ps)(runenv);
      oauthOpenIdDiscoveryUrl = await getStratoOAuthOpenIdDiscoveryUrl(ps)(
        runenv
      );
    }

    stratoConf = initStratoConfig({
      nodeUrl,
      eNodeUrl,
      oauthClientId,
      oauthClientSecret,
      oauthOpenIdDiscoveryUrl,
    });

    const eitherChainCommittee = await launchCommittee(stratoConf)(
      committeesTableName
    )(dynamoDB)(committee)();

    if (isLeft(eitherChainCommittee)) {
      throw Error("test failed");
    }

    committee = eitherChainCommittee.right;

    console.log("test res here", eitherChainCommittee);

    // Add contribution data
    for (const txn of unverifiedContributionsData) {
      txn.committeeId = committee.id;
      txn.id = dateToTxnId(txn.initiatedTimestamp);
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

  after(async () => {
    await deleteCommittee(committeesTableName)(dynamoDB)(committee);
  });
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
            finicityPaymentMethod: txn.finicityPaymentMethod,
            bankVerified: true,
          });
          reconCount++;
        }
      }
    }
    console.log("rec count", reconCount);
  };
