const Joi = require("joi");
const createStripeConnectUser = require("./createStripeConnectUser")

const onboardingSchema = Joi.object({
  code: Joi.string().required(),
});

module.exports = async (event, context) => {
  const res = onboardingSchema.validate(event.queryStringParameters);
  if (res.error) {
    return {
      statusCode: 400,
      body: JSON.stringify({
        message: res.error.message,
      }),
    };
  }

  const {
    code
  } = res.value;


  try {
    const id = await createStripeConnectUser(code);
    console.log(id)
    return {
      statusCode: 200,
      body: JSON.stringify({
        message: "success",
      }),
    };
  } catch (e) {
    return {
      statusCode: 400,
      body: JSON.stringify({
        message: "Code is not valid",
      }),
    };
  }
}
