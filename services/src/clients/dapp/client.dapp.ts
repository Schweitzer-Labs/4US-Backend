import { rest, util } from "blockapps-rest";

interface StratoNodeConfig {
  id: any;
  url: string;
  publicKey: string;
  port: number;
  oauth: {
    appTokenCookieName?: string;
    scope: string;
    appTokenCookieMaxAge: number;
    clientId: string;
    clientSecret: string;
    openIdDiscoveryUrl: string;
    redirectUri: string;
    logoutRedirectUri: string;
  };
}

export interface StratoSDKConfig {
  apiDebug: boolean;
  timeout: number;
  nodes: StratoNodeConfig[];
}

export const deployCommitteeContract = async (config: StratoSDKConfig) => {};
