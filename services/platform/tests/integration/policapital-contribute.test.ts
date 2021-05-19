import { expect } from "chai";

import lambdaHandler from "../../src/policapital-contribute";
import validContributeEvent from "../events/valid-contribute";
import invalidContributeEvent from "../events/invalid-contribute";

describe("Tests contribute lambda", function () {
  it("Stops a contribution call with an invalid payload", async () => {
    const result = await lambdaHandler(invalidContributeEvent);
    expect(result.statusCode).to.equal(400);

    let response = JSON.parse(result.body);
    expect(response).to.be.an("object");
    expect(response.message).to.be.an("string");
  });

  it("Allows a contribution call with a valid payload", async () => {
    const result = await lambdaHandler(validContributeEvent);
    expect(result.statusCode).to.equal(200);

    let response = JSON.parse(result.body);
    expect(response).to.be.an("object");
    expect(response.message).to.be.equal("success");
  });
});
