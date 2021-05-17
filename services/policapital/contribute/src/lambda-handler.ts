import { APIGatewayProxyEvent } from "aws-lambda";
import { Stripe } from "stripe";
import * as dotenv from "dotenv";
import { getStripeApiKey } from "./utils/config";
import { eventToPayment } from "./either-tasks/event-to-payment";

dotenv.config();

const runenv: any = process.env.RUNENV;
let stripe: Stripe;
let stripeApiKey: string;

export default async (event: APIGatewayProxyEvent) => {
  if (!stripeApiKey || !stripe) {
    stripeApiKey = await getStripeApiKey(runenv);
    stripe = new Stripe(stripeApiKey, {
      apiVersion: "2020-08-27",
    });
  }

  return eventToPayment(stripe)(event)();
};
