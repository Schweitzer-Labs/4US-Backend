import { taskEither as te } from "fp-ts";
import { pipe } from "fp-ts/function";
import { ApplicationError } from "../../utils/application-error";
import * as t from "io-ts";
import { TaskEither } from "fp-ts/TaskEither";
import { decodeError } from "../../utils/decode-error.util";

interface IStratoNodeConfig {
  id: any;
  url: string;
  publicKey?: string;
  oauth: {
    appTokenCookieName?: string;
    scope?: string;
    appTokenCookieMaxAge?: number;
    clientId: string;
    clientSecret: string;
    openIdDiscoveryUrl?: string;
    redirectUri?: string;
    logoutRedirectUri?: string;
  };
}

export interface IStratoSDKConfig {
  apiDebug: boolean;
  timeout: number;
  nodes: IStratoNodeConfig[];
  eNodeUrl: string;
}

export interface IInitStratoConfig {
  nodeUrl: string;
  eNodeUrl: string;
  oauthClientId: string;
  oauthClientSecret: string;
  oauthOpenIdDiscoveryUrl: string;
}

export const initStratoConfig = (
  conf: IInitStratoConfig
): IStratoSDKConfig => ({
  apiDebug: false,
  timeout: 600000,
  nodes: [
    {
      id: 0,
      url: conf.nodeUrl,
      oauth: {
        clientId: conf.oauthClientId,
        clientSecret: conf.oauthClientSecret,
        openIdDiscoveryUrl: conf.oauthOpenIdDiscoveryUrl,
      },
    },
  ],
  eNodeUrl: conf.eNodeUrl,
});

export const CreateUserResponse = t.type({
  token: t.string,
  address: t.string,
});

export type ICreateUserResponse = t.TypeOf<typeof CreateUserResponse>;

export const CommitTransactionResponse = t.unknown;

export type ICommitTransactionResponse = t.TypeOf<
  typeof CommitTransactionResponse
>;

export const decodeCreateUserResponse = (
  res: unknown
): TaskEither<ApplicationError, ICreateUserResponse> => {
  return pipe(
    te.fromEither(CreateUserResponse.decode(res)),
    te.mapLeft(decodeError("CreateUserResponse"))
  );
};

export const decodeCreateChainResponse = (
  res: unknown
): TaskEither<ApplicationError, string> => {
  return pipe(
    te.fromEither(t.string.decode(res)),
    te.mapLeft(decodeError("CreateChainResponse"))
  );
};
