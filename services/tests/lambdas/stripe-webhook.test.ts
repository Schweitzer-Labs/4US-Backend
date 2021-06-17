import { expect } from "chai";
import stripeWebHookLambda from "../../src/stripe-webhook.lambda";

describe("Stripe webhook", function () {
  it("Processes a webhook payload", async () => {
    const event = {
      data: {},
      type: "something",
    };
    const req: any = {
      body: JSON.stringify(event),
    };

    const res = await stripeWebHookLambda(req);

    expect(res.statusCode).to.equal(200);
  });
});
