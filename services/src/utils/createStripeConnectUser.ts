import { Stripe } from "stripe";
import { getStripeApiKey } from "./config";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
dotenv.config();

const runenv = process.env.RUNENV;

const ps = new AWS.SSM();

export const createStripeConnectUser = async (code) => {
  const stripeApiKey = await getStripeApiKey(ps)(runenv);

  const stripe = new Stripe(stripeApiKey, {
    apiVersion: "2020-08-27",
  });

  const { stripe_user_id: stripeUserId } = await stripe.oauth.token({
    grant_type: "authorization_code",
    code,
  });

  return stripeUserId;
};
