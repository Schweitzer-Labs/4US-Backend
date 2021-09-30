import { task, taskEither } from "fp-ts";
import { fold } from "fp-ts/TaskEither";
import { successResponse } from "../../src/utils/success-response";
import { runReportAndDecode } from "../../src/webhook/payout-paid/payout-paid.handler";
import { pipe } from "fp-ts/function";
import { getStripeApiKey } from "../../src/utils/config";
import { Stripe } from "stripe";
import * as AWS from "aws-sdk";
import * as dotenv from "dotenv";

dotenv.config();

const runenv: any = process.env.RUNENV;

describe("Reconciliation Report", function () {
  it("Pulls a report", async () => {
    const ps = new AWS.SSM();

    const stripeApiKey = await getStripeApiKey(ps)(runenv);
    const stripe = new Stripe(stripeApiKey, {
      apiVersion: "2020-08-27",
    });

    const res = await pipe(
      runReportAndDecode(stripe)(),
      fold(
        (error) => task.of(error.toResponse()),
        () => task.of(successResponse)
      )
    );
  });
});
