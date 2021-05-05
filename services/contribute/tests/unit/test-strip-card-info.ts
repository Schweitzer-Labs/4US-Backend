import { expect } from "chai";
import { stripCardInfo } from "../../src/utils/strip-card-info";

const validPayload = {
  amount: 400,
  cardCVC: "123",
  cardExpirationMonth: 12,
  cardExpirationYear: 2023,
  cardNumber: "4242424242424242",
  stripeUserId: "acct_1IdcwgRFrwYoR1VI",
};

const invalidPayload: any = {
  amount: 400,
  cardNumber: "4",
};

describe("Test config loader", function () {
  it("Strips card expiration month, year, and cvv from payload", async () => {
    const val: any = await stripCardInfo(validPayload);
    expect(val.cardExpirationMonth).to.be.undefined;
    expect(val.cardExpirationYear).to.be.undefined;
    expect(val.cardCVC).to.be.undefined;
  });

  it("Strips card number to last 4 digits", async () => {
    const val = await stripCardInfo(validPayload);
    expect(val.cardNumberLastFourDigits.length).to.equal(4);
  });

  it("Allows invalid object to pass through stripping", async () => {
    const val: any = await stripCardInfo(invalidPayload);
    expect(val.cardNumberLastFourDigits.length).to.equal(1);
  });
});
