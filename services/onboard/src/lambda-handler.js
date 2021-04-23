const Joi = require("joi");
const createStripeConnectUser = require("./createStripeConnectUser")


const headers = {
  "Access-Control-Allow-Headers" : "Content-Type",
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Methods": "OPTIONS,POST,GET"
}

const onboardingSchema = Joi.object({
  code: Joi.string().required(),
}).required();

module.exports = async (event, context) => {
  console.log("function invoked")
  const res = onboardingSchema.validate(event.queryStringParameters);
  if (res.error) {
    return {
      statusCode: 400,
      body: JSON.stringify({
        message: res.error.message,
      }),
      headers
    };
  }

  console.log("payload passed validation")

  const {
    code
  } = res.value;


  try {
    const id = await createStripeConnectUser(code);
    console.log("Stripe connect account request successful")
    console.log(id)
    return {
      statusCode: 200,
      body: JSON.stringify({
        message: "success",
      }),
      headers
    };
  } catch (e) {
    return {
      statusCode: 400,
      body: JSON.stringify({
        message: "Code is not valid",
      }),
      headers
    };
  }
}
