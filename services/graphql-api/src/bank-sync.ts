import { Stripe } from "stripe";
import * as dotenv from "dotenv";
import { getStripeApiKey } from "./utils/config";

dotenv.config();

const runenv: any = process.env.RUNENV;
let stripe: Stripe;
let stripeApiKey: any;

export default async () => {
  return "success";
};
