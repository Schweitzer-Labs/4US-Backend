import * as AWS from "aws-sdk";
import * as dotenv from "dotenv";

dotenv.config();

const ps = new AWS.SSM();

enum ConfigKeys {
  stripeApiKey = "/lambda/stripe/apikey",
}

const getConfig = async (env: string, name: string): Promise<string> => {
  console.log(env, name);
  switch (name) {
    case ConfigKeys.stripeApiKey:
      const fullKey = `/${env}${name}`;
      console.log("Config look up initiated on key: ", fullKey);
      const res = await ps
        .getParameter({
          Name: fullKey,
          WithDecryption: true,
        })
        .promise();
      return res.Parameter.Value;
    default:
      console.error("Config not found");
      throw new Error("Config not found");
  }
};

export const getStripeApiKey = async (env: string) =>
  await getConfig(env, ConfigKeys.stripeApiKey);
