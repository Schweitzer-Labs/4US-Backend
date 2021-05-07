import { expect } from "chai";
import { Finicity } from "../../src/clients/finicity.client";
import * as dotenv from "dotenv";

dotenv.config();

let finicity: Finicity;
let customerId = "1017099505";
let accountId = "1045029583";

describe("Test Finicity client", function () {
  before(() => {
    const FINICITY_PARTNER_ID: any = process.env.FINICITY_PARTNER_ID;
    const FINICITY_PARTNER_SECRET: any = process.env.FINICITY_PARTNER_SECRET;
    const FINICITY_APP_KEY: any = process.env.FINICITY_APP_KEY;
    const ONBOARDING_APP_URL: any = process.env.ONBOARDING_APP_URL;

    finicity = new Finicity(
      FINICITY_PARTNER_ID,
      FINICITY_PARTNER_SECRET,
      FINICITY_APP_KEY,
      ONBOARDING_APP_URL
    );
  });

  it("Pulls transactions by customer id and account id", async () => {
    const transactions = await finicity.getTransactions(
      customerId,
      accountId,
      1588365858,
      1598970681
    );
    expect(transactions).to.be.an("array");
  });
});
