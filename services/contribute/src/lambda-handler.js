const Joi = require("joi");
const { Stripe } = require("stripe");
const config = require("./config.js");
const { configKey } = require("./enums");
const stripCardInfo = require("./strip-card-info");
const { v4: uuidv4 } = require('uuid');

require('dotenv').config()

const runenv = process.env.RUNENV;

const AWS = require("aws-sdk");
AWS.config.update({ region: process.env.REGION });


const ddb = new AWS.DynamoDB({ apiVersion: "2012-08-10" })
    , tableName = process.env.DDBTABLE
;

const headers = {
  "Access-Control-Allow-Headers" : "Content-Type",
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Methods": "OPTIONS,POST,GET"
}


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

const writeDB = async (items) => {
  let params = {
    TableName: tableName,
    Item: items,
  };
  try {
    let data = await ddb.putItem(params).promise();
    return { message: "Success" };
  } catch (err) {
    throw (err);
  }
};

const ddbRecord = (message) => {
  console.log(message);
  let record = {};

  for (const attr in message) {
    const attrType = attr === "amount" ? "N" : "S";
    record[attr] = {};
    record[attr][attrType] = message[attr].toString();
  }
  return record;
};



let response;

module.exports = async (event, context) => {
  console.log("Contribute called")
  const parsedBody = JSON.parse(event.body);
  const res = contribSchema.validate(JSON.parse(event.body), {allowUnknown: true});
  if (res.error) {
    console.log("Validation failed", res.error)
    return {
      statusCode: 400,
      body: JSON.stringify({
        message: res.error.message,
      }),
      headers
    };
  }
  const strippedPayload = stripCardInfo(parsedBody)

  console.log("Validation succeeded")

  const {
    amount,
    cardNumber,
    cardExpirationMonth,
    cardExpirationYear,
    cardCVC,
    stripeUserId,
  } = res.value;

  try {
    console.log("Payment initiated")
    const stripePaymentIntentId = await executePayment(
      stripeUserId,
      amount,
      cardNumber,
      cardExpirationMonth,
      cardExpirationYear,
      cardCVC
    );


    console.log("Payment succeeded", {
      ...strippedPayload,
      stripePaymentIntentId
    });

    const row = {
        ...strippedPayload
      , stripePaymentIntentId
      , id: uuidv4()
    };

    try {
      const record = ddbRecord(row);
      console.log("writing", record);
      let written = await writeDB(record);
      console.log("Transaction recorded", written);

    } catch (err) {
      console.error("Failed to record transaction", row);
      console.error(err);
    }

    response = {
      statusCode: 200,
      body: JSON.stringify({
        message: "success",
      }),
      headers
    };
  } catch (err) {
    console.error("Payment failed", strippedPayload)
    console.error(err)
    return {
      statusCode: 401,
      body: JSON.stringify({
        message: "Payment failed",
      }),
      headers
    };
  }

  return response;
};
