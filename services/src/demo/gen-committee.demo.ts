import { IStratoSDKConfig } from "../clients/dapp/dapp.decoders";
import { launchCommittee } from "../clients/dapp/dapp.client";
import { isLeft } from "fp-ts/Either";
import { dateToTxnId, genTxnId } from "../utils/gen-txn-id.utils";
import { putTransaction } from "../utils/model/transaction/put-transaction.utils";
import { sleep } from "../utils/sleep.utils";
import { syncCommittee } from "../pipes/finicity-bank-sync.pipe";
import { ApplicationError } from "../utils/application-error";
import { decodeCSVAndSyncPayouts } from "../webhook/run-report-succeeded/report-run-succeeded.handler";
import { payoutReconcilationReportData } from "../../tests/seed/payout-reconcilation-report.data";
import { putCommittee } from "../utils/model/committee/put-committee.utils";
import { DynamoDB } from "aws-sdk";
import { disableFinicity } from "../utils/disable-finicity.utils";
import { FinicityConfig } from "../clients/finicity/finicity.decoders";
import { genCommittee } from "./utils/gen-committee.util";
import { unverifiedContributionsData } from "./data/unverified-contributions.data";
import { searchTransactions } from "../utils/model/transaction/search-transactions.query";
import { TransactionType } from "../utils/enums/transaction-type.enum";
import { isPayout } from "../pipes/reconcile-contributions.pipe";
import { groupTxnsByPayout } from "../utils/model/transaction/group-txns-by-payout.utils";
import { runRec } from "./utils/run-rec.util";
import { activateStripe } from "./utils/activate-stripe.util";
import { isClean, isReconciled } from "../utils/enums/demo-type.enum";
import { GenCommitteeInput } from "../graphql/input-types/gen-committee.input-type";
import { ICommittee } from "../model/committee.type";

export const genDemoCommittee =
  (comTable: string) =>
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (finConf: FinicityConfig) =>
  (stratoConf: IStratoSDKConfig) =>
  async (g: GenCommitteeInput): Promise<ICommittee> => {
    const stripeAccount = isClean(g.demoType)
      ? "acct_1IjTcsRC8iiQex3V"
      : genTxnId();
    const finConfig = isClean(g.demoType)
      ? {}
      : {
          finicityCustomerId: "5007489410",
          finicityAccountId: "5016000964",
        };

    let committee: ICommittee = genCommittee({
      stripeAccount,
      ...finConfig,
      candidateLastName: "Schweitzer",
      candidateFirstName: "Will",
      tzDatabaseName: "America/New_York",
      state: "ny",
      scope: "local",
      party: "republican",
      race: "general",
      district: "",
      county: "saratoga",
      officeType: "senator",
      ruleVersion: "nyboe-2020",
      efsFilerId: 15950,
      efsElectionId: 139,
      id: `will-schweitzer-${genTxnId()}`,
      bankName: "avanti",
    });

    const eitherChainCommittee = await launchCommittee(stratoConf)(comTable)(
      ddb
    )(committee)();

    if (isLeft(eitherChainCommittee)) {
      throw Error("test failed");
    }

    committee = eitherChainCommittee.right;

    // @ToDo break out fork via composition not conditional
    if (isClean(g.demoType)) return committee;

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
    const finRes = await syncCommittee(finConf)(txnTable)(ddb)(committee)();
    if (isLeft(finRes)) {
      throw new ApplicationError("sync failed", finRes.left);
    }

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
