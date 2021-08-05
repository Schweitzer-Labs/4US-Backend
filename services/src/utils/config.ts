import * as AWS from "aws-sdk";

enum ConfigKeys {
  stripeApiKey = "/lambda/stripe/apikey",
  lnUsername = "/lambda/ln/username",
  lnPassword = "/lambda/ln/password",
  finicityPartnerId = "/lambda/finicity/partnerId",
  finicityPartnerSecret = "/lambda/finicity/partnerSecret",
  finicityAppKey = "/lambda/finicity/appKey",
  stripeWebhookEndpointSecret = "/lambda/stripe/webhookEndpointSecret",
}

const retrieveConfig = (ps: AWS.SSM) => async (env: string, name: string) => {
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

const getConfig =
  (ps: AWS.SSM) =>
  async (env: string, name: string): Promise<string> => {
    console.log(`/${env}${name}`);
    switch (name) {
      case ConfigKeys.stripeApiKey:
      case ConfigKeys.lnUsername:
      case ConfigKeys.lnPassword:
      case ConfigKeys.finicityPartnerId:
      case ConfigKeys.finicityPartnerSecret:
      case ConfigKeys.finicityAppKey:
      case ConfigKeys.stripeWebhookEndpointSecret:
        return retrieveConfig(ps)(env, name);

      default:
        console.error("Config not found");
        throw new Error("Config not found");
    }
  };

export const getStripeApiKey = (ps: AWS.SSM) => async (env: string) =>
  await getConfig(ps)(env, ConfigKeys.stripeApiKey);

export const getStripeWebhookEndpointSecret =
  (ps: AWS.SSM) => async (env: string) =>
    await getConfig(ps)(env, ConfigKeys.stripeWebhookEndpointSecret);

export const getLNUsername = (ps: AWS.SSM) => async (env: string) =>
  await getConfig(ps)(env, ConfigKeys.lnUsername);

export const getLNPassword = (ps: AWS.SSM) => async (env: string) =>
  await getConfig(ps)(env, ConfigKeys.lnPassword);

export const getFinicityPartnerId = (ps: AWS.SSM) => async (env: string) =>
  await getConfig(ps)(env, ConfigKeys.finicityPartnerId);

export const getFinicityPartnerSecret = (ps: AWS.SSM) => async (env: string) =>
  await getConfig(ps)(env, ConfigKeys.finicityPartnerSecret);

export const getFinicityAppKey = (ps: AWS.SSM) => async (env: string) =>
  await getConfig(ps)(env, ConfigKeys.finicityAppKey);
