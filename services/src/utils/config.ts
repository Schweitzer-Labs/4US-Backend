import * as AWS from "aws-sdk";

enum ConfigKeys {
  stripeApiKey = "/lambda/stripe/apikey",
  lnUsername = "/lambda/ln/username",
  lnPassword = "/lambda/ln/password",
  finicityPartnerId = "/lambda/finicity/partnerId",
  finicityPartnerSecret = "/lambda/finicity/partnerSecret",
  finicityAppKey = "/lambda/finicity/appKey",
  stripeWebhookEndpointSecret = "/lambda/stripe/webhookEndpointSecret",
  stratoNodeUrl = "/lambda/strato/nodeUrl",
  stratoENodeUrl = "/lambda/strato/eNodeUrl",
  stratoOAuthClientId = "/lambda/strato/oauthClientId",
  stratoOAuthClientSecret = "/lambda/strato/oauthClientSecret",
  stratoOAuthOpenIdDiscoveryUrl = "/lambda/strato/oauthOpenIdDiscoveryUrl",
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
  async (env: string, configKey: ConfigKeys): Promise<string> => {
    console.log(`/${env}${configKey}`);
    return retrieveConfig(ps)(env, configKey);
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

export const getStratoNodeUrl = (ps: AWS.SSM) => async (env: string) =>
  await getConfig(ps)(env, ConfigKeys.stratoNodeUrl);

export const getStratoENodeUrl = (ps: AWS.SSM) => async (env: string) =>
  await getConfig(ps)(env, ConfigKeys.stratoENodeUrl);

export const getStratoOAuthClientId = (ps: AWS.SSM) => async (env: string) =>
  await getConfig(ps)(env, ConfigKeys.stratoOAuthClientId);

export const getStratoOauthClientSecret =
  (ps: AWS.SSM) => async (env: string) =>
    await getConfig(ps)(env, ConfigKeys.stratoOAuthClientSecret);

export const getStratoOAuthOpenIdDiscoveryUrl =
  (ps: AWS.SSM) => async (env: string) =>
    await getConfig(ps)(env, ConfigKeys.stratoOAuthOpenIdDiscoveryUrl);
