const Joi = require("joi");
const parseUa = require("./parse-ua.js");

const resHeaders = {
  "Access-Control-Allow-Headers" : "Content-Type",
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Methods": "OPTIONS,POST,GET"
}

const analyticsSchema = Joi.object({
  userAgent: Joi.string().required(),
  referrer: Joi.string(),
  event: Joi.string().required(),
}).required();

module.exports = async (event, context) => {
  console.log("function invoked")
  const res = analyticsSchema.validate(JSON.parse(event.body), {allowUnknown: true});
  if (res.error) {
    return {
      statusCode: 400,
      body: JSON.stringify({
        message: res.error.message,
      }),
      resHeaders
    };
  }

  const {
    userAgent,
    referrer,
  } = res.value

  console.log({
    headers: event.headers,
    userAgent: parseUa(userAgent),
    referrer,
    event: res.value.event
  })

  return {
    statusCode: 200,
    body: JSON.stringify({
      message: "success",
    }),
    resHeaders
  };
}
