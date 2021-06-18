import { expect } from "chai";

import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import * as dotenv from "dotenv";
import { genCommittee } from "../utils/gen-committee.util";
import { putCommittee } from "../../src/utils/model/put-committee.utils";
import { genTransaction } from "../utils/gen-transaction.util";
import { Direction } from "../../src/utils/enums/direction.enum";
import { TransactionType } from "../../src/utils/enums/transaction-type.enum";
import { putTransaction } from "../../src/utils/model/put-transaction.utils";
import { syncCommittee } from "../../src/pipes/finicity-bank-sync.pipe";
import { isLeft } from "fp-ts/Either";
import { ApplicationError } from "../../src/utils/application-error";
import { deleteCommittee } from "../../src/utils/model/delete-committee.utils";
import { sleep } from "../../src/utils/sleep.utils";
import { searchTransactions } from "../../src/queries/search-transactions.query";
import { task, taskEither } from "fp-ts";
import { pipe } from "fp-ts/function";
import * as FC from "../../src/clients/finicity/finicity.client";
import { milliToEpoch, now } from "../../src/utils/time.utils";
import { txnsToAgg } from "../../src/utils/model/txns-to-agg.utils";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

const config = {
  partnerId: process.env.FINICITY_PARTNER_ID,
  partnerSecret: process.env.FINICITY_PARTNER_SECRET,
  appKey: process.env.FINICITY_APP_KEY,
};

const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;

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

let syncRes;
let pTxns;
let fTxns;

describe("Syncs transactions with a platform account", function () {
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

    syncRes = await Promise.all(m.map((f) => f()).map(async (f) => await f));

    await sleep(1000);

    pTxns = await pipe(
      searchTransactions(txnsTableName)(dynamoDB)({
        committeeId: committee.id,
      }),
      taskEither.fold(
        (err) => task.of([]),
        (txns) => task.of(txns)
      )
    )();
    console.log(pTxns);

    fTxns = await pipe(
      FC.getTransactions(config)({
        customerId: committee.finicityCustomerId,
        accountId: committee.finicityAccountId,
        epochFrom: milliToEpoch(now()) - 60 * 60 * 24 * 30 * 6, // 6 months ago
        epochTo: milliToEpoch(now()),
      }),
      taskEither.fold(
        (err) => task.of([]),
        (txns) => task.of(txns)
      )
    )();
  });
  it("Count of transactions equal", async () => {
    expect(fTxns.length).to.equal(pTxns.length);
  });
  it("Total balance equal after sync", async () => {
    const fBalance = fTxns.reduce((acc, { amount }) => acc + amount, 0);
    const pAggs = txnsToAgg(pTxns);
    expect(Math.round(fBalance * 100)).to.equal(pAggs.balance);
  });

  it("Transaction amounts are converted to integers", async () => {
    const nonIntegers = pTxns.filter(({ amount }) => {
      console.log(amount);
      return !Number.isInteger(amount);
    });
    expect(nonIntegers.length).to.equal(0);
  });

  it("Check number is a proper string when set", async () => {
    const badCheckNumber = pTxns.filter(({ checkNumber }) => {
      return checkNumber === "undefined";
    });

    expect(badCheckNumber.length).to.equal(0);
  });

  after(async () => {
    await deleteCommittee(committeesTableName)(dynamoDB)(committee);
  });
});
