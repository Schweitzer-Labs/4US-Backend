import { expect } from "chai";
import * as Finicity from "../../src/clients/finicity/finicity.client";
import * as dotenv from "dotenv";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";

dotenv.config();

let customerId = "1017099505";
let accountId = "1045029583";
let epochFrom = 1588365858;
let epochTo = 1598970681;

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
      Finicity.getTransactions(config)(
        customerId,
        accountId,
        epochFrom,
        epochTo
      ),
      taskEither.fold(
        () => task.of([]),
        (res) => task.of(res)
      )
    )();
    console.log(transactions);
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
