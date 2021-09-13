import { expect } from "chai";
import * as Finicity from "../../src/clients/finicity/finicity.client";
import * as dotenv from "dotenv";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";
import { now, milliToEpoch } from "../../src/utils/time.utils";

dotenv.config();

let customerId = "5007489410";
let accountId = "5016000964";
let epochFrom = milliToEpoch(now()) - 60 * 60 * 24 * 30 * 6; // 6 months ago
let epochTo = milliToEpoch(now());

let config: any;

describe("Test Finicity client", function () {
  before(() => {
    config = {
      partnerId: process.env.FINICITY_PARTNER_ID,
      partnerSecret: process.env.FINICITY_PARTNER_SECRET,
      appKey: process.env.FINICITY_APP_KEY,
    };
  });

  it("Pulls transactions by customer id and account id", async () => {
    const transactions = await pipe(
      Finicity.getTransactions(config)({
        customerId,
        accountId,
        epochFrom,
        epochTo,
      }),
      taskEither.fold(
        () => task.of([]),
        (res) => task.of(res)
      )
    )();

    expect(transactions[0].amount).to.be.an("number");
  });

  it("Rejects call with invalid customer id and account id", async () => {
    const res = await pipe(
      Finicity.getTransactions(config)("00000", "00000", epochFrom, epochTo),
      taskEither.fold(
        (err) => task.of(err.message),
        (res) => task.of("success")
      )
    )();
    expect(res).to.equal("Get Finicity Transactions request failed");
  });
});
