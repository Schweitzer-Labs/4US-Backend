import { Stripe } from "stripe";
import * as dotenv from "dotenv";
import { getStripeApiKey } from "./utils/config";

dotenv.config();

const runenv: any = process.env.RUNENV;
let stripe: Stripe;
let stripeApiKey: any;

export default async () => {
    if (!stripeApiKey || !stripe) {
        stripeApiKey = await getStripeApiKey(runenv);
        stripe = new Stripe(stripeApiKey, {
            apiVersion: "2020-08-27",
        });
    }

    return "success";
};
