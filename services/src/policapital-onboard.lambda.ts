import Joi from "joi";
import { createStripeConnectUser } from "./utils/createStripeConnectUser";
import { headers } from "./utils/headers";
import * as dotenv from "dotenv";

dotenv.config();
const corsOrigin = process.env.CORS_ORIGIN;

const onboardingSchema = Joi.object({
  code: Joi.string().required(),
}).required();

export default async (event, context) => {
  console.log(JSON.stringify(event));
  const res = onboardingSchema.validate(event.queryStringParameters);
  if (res.error) {
    return {
      statusCode: 400,
      body: JSON.stringify({
        message: res.error.message,
      }),
      headers,
    };
  }

  console.log("payload passed validation");

  const { code } = res.value;

  try {
    const id = await createStripeConnectUser(code);
    console.log("Stripe connect account request successful");
    return {
      statusCode: 200,
      body: JSON.stringify({
        message: "success",
      }),
      headers: headers(corsOrigin),
    };
  } catch (e) {
    return {
      statusCode: 400,
      body: JSON.stringify({
        message: "Code is not valid",
      }),
      headers: headers(corsOrigin),
    };
  }
};
