import { expect } from "chai";
import contribute from "../../src/policapital-contribute.lambda";
import { invalidPolicapitalContributeProxy } from "../events/invalid-policapital-contribute.proxy";
import { validPolicapitalContributeProxy } from "../events/valid-policapital-contribute-proxy";
import { seedCommittees } from "../seed/seed-committees.job";

describe("Policapital Contribute Lambda", function () {
  it("Stops a contribution call with an invalid payload", async () => {
    const result = await contribute(invalidPolicapitalContributeProxy);
    expect(result.statusCode).to.equal(400);

    let response = JSON.parse(result.body);
    expect(response).to.be.an("object");
    expect(response.message).to.be.an("string");
  });

  it("Allows a contribution call with a valid payload", async () => {
    const result = await contribute(validPolicapitalContributeProxy);
    expect(result.statusCode).to.equal(200);

    let response = JSON.parse(result.body);
    expect(response).to.be.an("object");
    expect(response.message).to.be.equal("success");
  });
});
