import { taskEither as te } from "fp-ts";
import { pipe } from "fp-ts/function";
import { ApplicationError } from "../../utils/application-error";
import * as t from "io-ts";
import { TaskEither } from "fp-ts/TaskEither";
import { decodeError } from "../../utils/decode-error.util";
import { ICommittee } from "../../queries/get-committee-by-id.query";
import { Config } from "blockapps-rest";
import { dashToUnderscore } from "../../utils/dash-to-underscore.utils";

export interface IStratoSDKConfig {
  config: Config;
  eNodeUrl: string;
}

export interface IInitStratoConfig {
  nodeUrl: string;
  eNodeUrl: string;
  oauthClientId: string;
  oauthClientSecret: string;
  oauthOpenIdDiscoveryUrl: string;
}

export const initStratoConfig = (conf: IInitStratoConfig): IStratoSDKConfig => {
  const oauth: any = {
    clientId: conf.oauthClientId,
    clientSecret: conf.oauthClientSecret,
    openIdDiscoveryUrl: conf.oauthOpenIdDiscoveryUrl,
    scope: "https://4us/strato/dummy",
  };

  const config = {
    apiDebug: false,
    timeout: 600000,
    nodes: [
      {
        url: conf.nodeUrl,
        oauth,
      },
    ],
  };

  return {
    config,
    eNodeUrl: conf.eNodeUrl,
  };
};

export const CreateUserResponse = t.type({
  token: t.string,
  address: t.string,
});

export type ICreateUserResponse = t.TypeOf<typeof CreateUserResponse>;

export const CommitTransactionResponse = t.unknown;

export type ICommitTransactionResponse = t.TypeOf<
  typeof CommitTransactionResponse
>;

export const initializeCommitteeResponse = t.unknown;

export type IInitializeCommitteeResponse = t.TypeOf<
  typeof initializeCommitteeResponse
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

export const getContractName =
  (contractName: string) => (committee: ICommittee) =>
    `${contractName}_${dashToUnderscore(committee.id)}`;
