import {
  getStratoENodeUrl,
  getStratoNodeUrl,
  getStratoOAuthClientId,
  getStratoOauthClientSecret,
  getStratoOAuthOpenIdDiscoveryUrl,
} from "../utils/config";
import {
  initStratoConfig,
  IStratoSDKConfig,
} from "../clients/dapp/dapp.decoders";
import { launchCommittee } from "../clients/dapp/dapp.client";
import { isLeft } from "fp-ts/Either";
import { unverifiedContributionsData } from "../../tests/seed/unverified-contributions.data";
import { dateToTxnId, genTxnId } from "../utils/gen-txn-id.utils";
import { putTransaction } from "../utils/model/put-transaction.utils";
import { sleep } from "../utils/sleep.utils";
import { syncCommittee } from "../pipes/finicity-bank-sync.pipe";
import { ApplicationError } from "../utils/application-error";
import { decodeCSVAndSyncPayouts } from "../webhook/run-report-succeeded/report-run-succeeded.handler";
import { payoutReconcilationReportData } from "../../tests/seed/payout-reconcilation-report.data";
import { putCommittee } from "../utils/model/put-committee.utils";
import { DynamoDB } from "aws-sdk";
import { genCommittee } from "../../tests/utils/gen-committee.util";

const stripeAccount = genTxnId();

export const genDemoCommittee =
  (comTable: string) =>
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (stratoConf: IStratoSDKConfig) => {
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

    const eitherChainCommittee = await launchCommittee(stratoConf)(comTable)(
      ddb
    )(committee)();

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
      await putTransaction(txnTable)(ddb)(txn);
    }

    await sleep(1000);

    // Sync finicity data
    const lazyRes = await syncCommittee()(txnsTableName)(dynamoDB)(committee)();
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
  };
