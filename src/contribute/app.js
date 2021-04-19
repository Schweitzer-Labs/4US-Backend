// const axios = require('axios')
// const url = 'http://checkip.amazonaws.com/';
const Joi = require("joi");
const { Stripe } = require("stripe");

const stripeApiKey =
  "sk_test_51HsJE2EUhH8cxK5gaCBDDbs6B5mYGjrctrlX73pJ1iaNIEcphQg5L4Qkrri73owunIkrgDn0yQI5ibbgleYAe7hx00JMRiXEzd";

const stripeUserId = "acct_1IPBGPRDfW8UMKAc";

const stripe = new Stripe(stripeApiKey, {
  apiVersion: "2020-08-27",
});

const executePayment = async (
  stripeAccount,
  amount,
  cardNumber,
  cardExpirationMonth,
  cardExpirationYear,
  cardCVC
) => {
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
});

let response;

exports.lambdaHandler = async (event, context) => {
  const res = contribSchema.validate(JSON.parse(event.body));

  if (res.error) {
    return {
      statusCode: 400,
      body: JSON.stringify({
        message: res.error.message,
      }),
    };
  }

  const {
    amount,
    cardNumber,
    cardExpirationMonth,
    cardExpirationYear,
    cardCVC,
  } = res.value;

  try {
    const res = await executePayment(
      stripeUserId,
      amount,
      cardNumber,
      cardExpirationMonth,
      cardExpirationYear,
      cardCVC
    );
    console.log(res);
    response = {
      statusCode: 200,
      body: JSON.stringify({
        message: "success",
      }),
    };
  } catch (err) {
    return err;
  }

  return response;
};
