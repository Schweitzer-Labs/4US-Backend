"use strict";

const lambdaHandler = require("../src/lambda-handler.js");
const invalidCode = require('./events/invalid-code.json')
const missingQs = require('./events/missing-qs.json')
const chai = require("chai");
const expect = chai.expect;

describe("Tests contribute lambda", function () {
  it("Fails to process query string containing invalid code", async () => {
    const result = await lambdaHandler(invalidCode, context);
    expect(result.statusCode).to.equal(400);
    const body = JSON.parse(result.body)
    expect(body.message).to.equal("Code is not valid");
  });

  it("Fails to process payload missing a query string", async () => {
    const result = await lambdaHandler(missingQs, context);
    expect(result.statusCode).to.equal(400);
  });
});
