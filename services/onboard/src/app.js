const Joi = require("joi");

const onboardingSchema = Joi.object({
  code: Joi.string().required(),
});


exports.lambdaHandler = async (event, context) => {
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

  return {
    statusCode: 200,
    body: JSON.stringify({
      message: "success",
    }),
  };
}
