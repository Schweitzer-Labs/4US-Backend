import Joi from "joi";
import { parseUA } from "./utils/parse-ua";
import headers from "./utils/headers";
import { APIGatewayProxyResult } from "aws-lambda";

const analyticsSchema = Joi.object({
  userAgent: Joi.string().required(),
  referrer: Joi.string(),
  event: Joi.string().required(),
  src: Joi.string().required(),
}).required();

export default async (event, context): Promise<APIGatewayProxyResult> => {
  console.log(JSON.stringify(event));
  const res = analyticsSchema.validate(JSON.parse(event.body), {
    allowUnknown: true,
  });
  if (res.error) {
    return {
      statusCode: 400,
      body: JSON.stringify({
        message: res.error.message,
      }),
      headers,
    };
  }

  const { userAgent, referrer, src } = res.value;

  const logItem = {
    headers: event.headers,
    userAgent: parseUA(userAgent),
    referrer,
    src,
    event: res.value.event,
  };

  console.log(logItem);

  return {
    statusCode: 200,
    body: JSON.stringify({
      message: "success",
    }),
    headers,
  };
};
