import { expect } from "chai";
import { getStripeApiKey } from "../../src/utils/config";

describe("Test config loader", function () {
  it("Loads config from parameter store", async () => {
    const val = await getStripeApiKey("dev");
    expect(val).to.be.an("string");
  });
});
