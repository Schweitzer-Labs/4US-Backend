"use strict";

const config = require("../../src/config.js");
const { configKey } = require("../../src/enums");
const chai = require("chai");
const expect = chai.expect;

describe("Test config loader", function () {
  it("Loads config from parameter store", async () => {
    const val = await config.get("dev", configKey.stripeApiKey);
    expect(val).to.be.an("string");
  });
});
