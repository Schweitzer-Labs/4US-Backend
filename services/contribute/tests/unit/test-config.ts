import { expect } from "chai";


import config from "../../src/config";
import keys from "../../src/enums";


describe("Test config loader", function () {
  it("Loads config from parameter store", async () => {
    const val = await config.get("dev", keys.stripeApiKey);
    expect(val).to.be.an("string");
  });
});
