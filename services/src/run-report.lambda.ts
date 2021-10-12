import * as AWS from "aws-sdk";

import * as dotenv from "dotenv";
import { isLeft } from "fp-ts/Either";
import { Stripe } from "stripe";
import { runReportAndDecode } from "./webhook/payout-paid/payout-paid.handler";
import { getStripeApiKey } from "./utils/config";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const runenv: any = process.env.RUNENV;
let stripe: Stripe;
let stripeApiKey: string;
const ps = new AWS.SSM();

export default async (event: any) => {
  console.log("Event: ", event);
  if (!stripeApiKey || !stripe) {
    console.log("Setting up configuration");
    stripeApiKey = await getStripeApiKey(ps)(runenv);
    stripe = new Stripe(stripeApiKey, {
      apiVersion: "2020-08-27",
    });
    console.log("Configuration values have been set");
  }

  const res = await runReportAndDecode(stripe)(event.connectAccountId)();
  if (isLeft(res)) throw res.left;

  console.log("Run report succeeded", res.right);
  return "success";
};
