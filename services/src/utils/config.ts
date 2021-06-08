import * as AWS from "aws-sdk";

const ps = new AWS.SSM();

enum ConfigKeys {
  stripeApiKey = "/lambda/stripe/apikey",
  lnUsername = "/lambda/ln/username",
  lnPassword = "/lambda/ln/password",
  finicityPartnerId = "/lambda/finicity/partnerId",
  finicityPartnerSecret = "/lambda/finicity/partnerSecret",
  finicityAppKey = "/lambda/finicity/appKey",
}

const retrieveConfig = async (env: string, name: string) => {
  const fullKey = `/${env}${name}`;
  console.log("Config look up initiated on key: ", fullKey);
  const res = await ps
    .getParameter({
      Name: fullKey,
      WithDecryption: true,
    })
    .promise();
  return res.Parameter.Value;
};

const getConfig = async (env: string, name: string): Promise<string> => {
  console.log(`/${env}${name}`);
  switch (name) {
    case ConfigKeys.stripeApiKey:
    case ConfigKeys.lnUsername:
    case ConfigKeys.lnPassword:
    case ConfigKeys.finicityPartnerId:
    case ConfigKeys.finicityPartnerSecret:
    case ConfigKeys.finicityAppKey:
      return retrieveConfig(env, name);

    default:
      console.error("Config not found");
      throw new Error("Config not found");
  }
};

export const getStripeApiKey = async (env: string) =>
  await getConfig(env, ConfigKeys.stripeApiKey);

export const getLNUsername = async (env: string) =>
  await getConfig(env, ConfigKeys.lnUsername);

export const getLNPassword = async (env: string) =>
  await getConfig(env, ConfigKeys.lnPassword);

export const getFinicityPartnerId = async (env: string) =>
  await getConfig(env, ConfigKeys.finicityPartnerId);

export const getFinicityPartnerSecret = async (env: string) =>
  await getConfig(env, ConfigKeys.finicityPartnerSecret);

export const getFinicityAppKey = async (env: string) =>
  await getConfig(env, ConfigKeys.finicityAppKey);
