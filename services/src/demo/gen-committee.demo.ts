import { IStratoSDKConfig } from "../clients/dapp/dapp.decoders";
import { launchCommittee } from "../clients/dapp/dapp.client";
import { isLeft } from "fp-ts/Either";
import { dateToTxnId, genTxnId } from "../utils/gen-txn-id.utils";
import { putTransaction } from "../utils/model/put-transaction.utils";
import { sleep } from "../utils/sleep.utils";
import { syncCommittee } from "../pipes/finicity-bank-sync.pipe";
import { ApplicationError } from "../utils/application-error";
import { decodeCSVAndSyncPayouts } from "../webhook/run-report-succeeded/report-run-succeeded.handler";
import { payoutReconcilationReportData } from "../../tests/seed/payout-reconcilation-report.data";
import { putCommittee } from "../utils/model/put-committee.utils";
import { DynamoDB } from "aws-sdk";
import { disableFinicity } from "../utils/disable-finicity.utils";
import { FinicityConfig } from "../clients/finicity/finicity.decoders";
import { ICommittee } from "../queries/get-committee-by-id.query";
import { genCommittee } from "./utils/gen-committee.util";
import { unverifiedContributionsData } from "./data/unverified-contributions.data";
import { searchTransactions } from "../queries/search-transactions.query";
import { TransactionType } from "../utils/enums/transaction-type.enum";
import { isPayout } from "../pipes/reconcile-contributions.pipe";
import { groupTxnsByPayout } from "../utils/model/group-txns-by-payout.utils";
import { runRec } from "./utils/run-rec.util";
import { activateStripe } from "./utils/activate-stripe.util";

const stripeAccount = genTxnId();

export const genDemoCommittee =
  (comTable: string) =>
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (finConf: FinicityConfig) =>
  async (stratoConf: IStratoSDKConfig): Promise<ICommittee> => {
    let committee: ICommittee = genCommittee({
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
      txn.donorVerificationScore = 20;
      txn.state = txn.state.toUpperCase();
      await putTransaction(txnTable)(ddb)(txn);
    }

    // Sync finicity data
    const lazyRes = await syncCommittee(finConf)(txnTable)(ddb)(committee)();
    if (isLeft(lazyRes)) {
      throw new ApplicationError("sync failed", lazyRes.left);
    }
    const m = lazyRes.right;
    await Promise.all(m.map((f) => f()).map(async (f) => await f));

    // Sync payout data
    const syncRes = await decodeCSVAndSyncPayouts(txnTable)(comTable)(ddb)(
      stripeAccount
    )(payoutReconcilationReportData)();
    if (isLeft(syncRes)) {
      throw new ApplicationError("Payout sync failed", syncRes.left);
    }
    await putCommittee(comTable)(ddb)(disableFinicity(committee));

    // Run Reconciliation process

    const txnsRes = await searchTransactions(txnTable)(ddb)({
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

    await runRec(txnTable)(ddb)(payoutGroups)(bankPayouts);

    await putCommittee(comTable)(ddb)(activateStripe(committee));

    return committee;
  };
