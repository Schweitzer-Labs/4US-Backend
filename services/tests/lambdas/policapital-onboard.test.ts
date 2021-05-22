import { expect } from "chai";
import onboard from '../../src/policapital-onboard.lambda'

import {invalidPolicapitalOnboardCode} from "../events/invalid-policapital-onboard-code.proxy";
import {missingPolicapitalOnboardQS} from "../events/missing-policapital-onboard-qs.proxy";

describe("Tests contribute lambda", function () {
  it("Fails to process query string containing invalid code", async () => {
    const result = await onboard(invalidPolicapitalOnboardCode, context);
    expect(result.statusCode).to.equal(400);
    const body = JSON.parse(result.body)
    expect(body.message).to.equal("Code is not valid");
  });

  it("Fails to process payload missing a query string", async () => {
    const result = await onboard(missingPolicapitalOnboardQS, context);
    expect(result.statusCode).to.equal(400);
  });
});
