import { expect } from "chai";
import analytics from "../../src/politcapital-analytics.lambda";
import {validPolitcapitalAnalyticsProxy} from "../events/valid-policapital-analytics.proxy";
import {invalidPolicapitalAnalyticsProxy} from "../events/invalid-policapital-analytics.proxy";

describe("Policapital Analytics Lambda", function () {
  it("Accepts a valid analytics object", async () => {
    const result = await analytics(validPolitcapitalAnalyticsProxy, context);
    expect(result.statusCode).to.equal(200);
  });

  it("Rejects an invalid payload", async () => {
    const result = await analytics(invalidPolicapitalAnalyticsProxy, context);
    expect(result.statusCode).to.equal(400);
  });
});
