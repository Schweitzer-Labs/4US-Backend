const { Stripe } = require("stripe");

require('dotenv').config()

const runenv = process.env.RUNENV

module.exports = async (code) => {
  const {
    stripe_user_id: stripeUserId,
  } = await this.stripe.oauth.token({
    grant_type: 'authorization_code',
    code,
  });

  return { stripeUserId };
}
