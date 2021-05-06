const { Stripe } = require("stripe");
const config = require("./config.js");
const { configKey } = require("./enums");

require('dotenv').config()

const runenv = process.env.RUNENV

module.exports = async code => {
  const stripeApiKey = await config.get(runenv, configKey.stripeApiKey);

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
