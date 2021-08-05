import { expect } from "chai";
import { getStripeApiKey } from "../../src/utils/config";
import * as AWS from "aws-sdk";

const ps = new AWS.SSM();

describe("Test config loader", function () {
  it("Loads config from parameter store", async () => {
    const val = await getStripeApiKey(ps)("qa");
    expect(val).to.be.an("string");
  });
});
