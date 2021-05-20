import { Stripe } from "stripe";
import {getStripeApiKey} from "./config";
import * as dotenv from 'dotenv'
dotenv.config()

const runenv = process.env.RUNENV

export const createStripeConnectUser = async code => {
  const stripeApiKey = await getStripeApiKey(runenv);

  const stripe = new Stripe(stripeApiKey, {
    apiVersion: "2020-08-27",
  });

  const {
    stripe_user_id: stripeUserId,
  } = await stripe.oauth.token({
    grant_type: 'authorization_code',
    code,
  });

  return stripeUserId;
}
