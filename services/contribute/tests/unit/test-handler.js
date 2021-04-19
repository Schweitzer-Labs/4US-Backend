"use strict";

const app = require("../../src/app.js");
const validContributeEvent = require("../events/valid-contribute.json");
const invalidContributeEvent = require("../events/invalid-contribute.json");
const chai = require("chai");
const expect = chai.expect;

describe("Tests contribute lambda", function () {
  it("Stops a contribution call with an invalid payload", async () => {
    const result = await app.lambdaHandler(invalidContributeEvent, context);
    expect(result.statusCode).to.equal(400);

    let response = JSON.parse(result.body);
    expect(response).to.be.an("object");
    expect(response.message).to.be.an("string");
  });

  it("Allows a contribution call with a valid payload", async () => {
    const result = await app.lambdaHandler(validContributeEvent, context);
    expect(result.statusCode).to.equal(200);

    let response = JSON.parse(result.body);
    expect(response).to.be.an("object");
    expect(response.message).to.be.equal("success");
  });
});
