"use strict";

const lambdaHandler = require("../src/lambda-handler.js");
const invalidObject = require('./events/invalid-object.json')
const validObject = require('./events/valid-object.json')
const chai = require("chai");
const expect = chai.expect;

describe("Tests analytics lambda", function () {
  it("Accepts a valid analytics object", async () => {
    const result = await lambdaHandler(validObject, context);
    expect(result.statusCode).to.equal(200);
  });

  it("Rejects an invalid payload", async () => {
    const result = await lambdaHandler(invalidObject, context);
    expect(result.statusCode).to.equal(400);
  });
});
