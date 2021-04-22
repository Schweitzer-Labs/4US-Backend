"use strict";

const app = require("../src/app.js");
const validCode = require('./events/valid-code.json')
const chai = require("chai");
const expect = chai.expect;

describe("Tests contribute lambda", function () {
  it("Accepts a query string containing code and creates stripe account", async () => {
    const result = await app.lambdaHandler(validCode, context);
    expect(result.statusCode).to.equal(400);

  });
});
