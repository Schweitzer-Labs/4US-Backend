const Joi = require("joi");
const { Stripe } = require("stripe");
const config = require("./config.js");
const { configKey } = require("./enums");


require('dotenv').config()

const runenv = process.env.RUNENV

const executePayment = async (
  stripeAccount,
  amount,
  cardNumber,
  cardExpirationMonth,
  cardExpirationYear,
  cardCVC
) => {
  const stripeApiKey = await config.get(runenv, configKey.stripeApiKey);

  const stripe = new Stripe(stripeApiKey, {
    apiVersion: "2020-08-27",
  });

  const methodRes = await stripe.paymentMethods.create({
    type: "card",
    card: {
      number: cardNumber,
      exp_month: cardExpirationMonth,
      exp_year: cardExpirationYear,
      cvc: cardCVC,
    },
  });

  const paymentMethodId = methodRes.id;

  const res = await stripe.paymentIntents.create({
    amount,
    currency: "usd",
    payment_method: paymentMethodId,
    confirm: true,
    transfer_data: {
      destination: stripeAccount,
    },
  });

  const { id: stripeTxnId } = res;

  return stripeTxnId;
};

const contribSchema = Joi.object({
  cardNumber: Joi.string().required(),
  cardExpirationMonth: Joi.number().required(),
  cardExpirationYear: Joi.number().required(),
  cardCVC: Joi.string().required(),
  amount: Joi.number().required(),
  stripeUserId: Joi.string().required(),
});

let response;

module.exports = async (event, context) => {
  console.log("contribute called")
  const res = contribSchema.validate(JSON.parse(event.body));
  if (res.error) {
    console.log("validation failed")
    return {
      statusCode: 400,
      body: JSON.stringify({
        message: res.error.message,
      }),
    };
  }

  console.log("validation succeeded")

  const {
    amount,
    cardNumber,
    cardExpirationMonth,
    cardExpirationYear,
    cardCVC,
    stripeUserId,
  } = res.value;

  try {
    console.log("payment initiated")
    await executePayment(
      stripeUserId,
      amount,
      cardNumber,
      cardExpirationMonth,
      cardExpirationYear,
      cardCVC
    );

    console.log("payment succeeded")

    response = {
      statusCode: 200,
      body: JSON.stringify({
        message: "success",
      }),
    };
  } catch (err) {
    console.error("payment failed")
    console.error(err)
    return {
      statusCode: 401,
      body: JSON.stringify({
        message: "payment failed",
      }),
    };
  }

  return response;
};
