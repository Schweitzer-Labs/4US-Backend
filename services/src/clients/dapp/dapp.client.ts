import { rest, util, oauthUtil } from "blockapps-rest";
import { ICommittee } from "../../queries/get-committee-by-id.query";
import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../queries/search-transactions.decoder";
import { pipe } from "fp-ts/function";
import { taskEither as te } from "fp-ts";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";

import {
  decodeCreateChainResponse,
  decodeCreateUserResponse,
  ICommitTransactionResponse,
  ICreateUserResponse,
  IStratoSDKConfig,
} from "./dapp.decoders";
import { putCommitteeAndDecode } from "../../utils/model/put-committee.utils";
import { putTransactionAndDecode } from "../../utils/model/put-transaction.utils";
import { committeeContract } from "./committee.contract";
import { sleep } from "../../utils/sleep.utils";

export const getClientUser = async (config: IStratoSDKConfig) => {
  const options = { config };
  const oauth = oauthUtil.init(config.nodes[0].oauth);
  const tokenResponse = await oauth.getAccessTokenByClientSecret();
  const oauthUser = { token: tokenResponse.token.access_token };

  return await rest.createUser(oauthUser, options);
};

export const getClientUserAndDecode = (
  config: IStratoSDKConfig
): TaskEither<ApplicationError, ICreateUserResponse> =>
  pipe(
    te.tryCatch(
      () => getClientUser(config),
      (e) => new ApplicationError("Strato create user failed", e)
    ),
    te.chain(decodeCreateUserResponse)
  );

export const deployCommitteeChain =
  (config: IStratoSDKConfig) =>
  (committee: ICommittee) =>
  async (user: ICreateUserResponse) => {
    const dappUser = await getClientUser(config);
    const options = { config, isDetailed: true };

    const chainArgs = {
      label: "CommitteeContract",
      name: "CommitteeContract",
      src: committeeContract,
      args: { _a: 10 },
      members: [
        {
          address: dappUser.address,
          enode: config.eNodeUrl,
        },
      ],
      balances: [
        {
          address: dappUser.address,
          balance: 100000000000000000000000,
        },
      ],
    };

    const chain = await rest.createChain(
      dappUser,
      chainArgs,
      { name: "CommitteeContract" },
      options
    );

    return chain;
  };

const deployCommitteeChainAndDecode =
  (config: IStratoSDKConfig) =>
  (committee: ICommittee) =>
  (user: ICreateUserResponse): TaskEither<ApplicationError, string> =>
    pipe(
      te.tryCatch(
        () => deployCommitteeChain(config)(committee)(user),
        (e) => new ApplicationError("Strato chain deployment failed", e)
      ),
      te.chain(decodeCreateChainResponse)
    );

const addChainToCommittee =
  (committee) =>
  (chainId: string): ICommittee => ({
    ...committee,
    chainId,
  });

const commitTransactionToChain =
  (config: IStratoSDKConfig) =>
  (committee: ICommittee) =>
  (t: ITransaction) =>
  async (user: ICreateUserResponse): Promise<any> => {
    console.log("strato config", config);
    const contract = {
      name: "CommitteeContract",
      address: "0000000000000000000000000000000000000100",
    };
    const options = {
      config,
      isDetailed: true,
      chainIds: [committee.chainId],
    };
    const callArgs = {
      contract,
      method: "commitTransactionAndGetIndex",
      args: {
        _id: t.id,
        _committeeId: t.committeeId,
        _direction: t.direction,
        _amount: t.amount,
        _paymentMethod: t.paymentMethod,
        _bankVerified: t.bankVerified,
        _ruleVerified: t.ruleVerified,
        _initiatedTimestamp: t.initiatedTimestamp,
        _source: t.source,
      },
    };
    const res = await rest.call(user, callArgs, options);

    console.log("blockchain res", res);

    return res;
  };

const commitTransactionToChainAndDecode =
  (config: IStratoSDKConfig) =>
  (committee: ICommittee) =>
  (txn: ITransaction) =>
  (
    user: ICreateUserResponse
  ): TaskEither<ApplicationError, ICommitTransactionResponse> =>
    pipe(
      te.tryCatch(
        () => commitTransactionToChain(config)(committee)(txn)(user),
        (e) => new ApplicationError("Strato commit transaction failed", e)
      )
    );

const addBlockchainMetadataToTransaction =
  (txn: ITransaction) =>
  (commitTransactionsRes: ICommitTransactionResponse): ITransaction => {
    const newTxn = {
      ...txn,
      blockchainMetadata: commitTransactionsRes,
    };
    console.log("new txn", newTxn);

    return newTxn;
  };

export const callMethodOnContract =
  (config: IStratoSDKConfig) =>
  (committee: ICommittee) =>
  async (user: ICreateUserResponse) => {
    const contract = {
      name: "CommitteeContract",
      address: "0000000000000000000000000000000000000100",
    };
    const options = {
      config,
      isDetailed: true,
      chainIds: [committee.chainId],
    };
    const callArgs = {
      contract,
      method: "store",
      args: {
        num: 55555,
      },
    };

    const res = await rest.call(user, callArgs, options);

    await sleep(1000);
    return res;
  };

export const launchCommittee =
  (config: IStratoSDKConfig) =>
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committee: ICommittee): TaskEither<ApplicationError, ICommittee> =>
    pipe(
      getClientUserAndDecode(config),
      te.chain(deployCommitteeChainAndDecode(config)(committee)),
      te.map(addChainToCommittee(committee)),
      te.chain(putCommitteeAndDecode(committeeTableName)(dynamoDB))
    );

export const commitTransaction =
  (config: IStratoSDKConfig) =>
  (txnsTablesName: string) =>
  (dynamoDB: DynamoDB) =>
  (committee: ICommittee) =>
  (txn: ITransaction): TaskEither<ApplicationError, ITransaction> =>
    pipe(
      getClientUserAndDecode(config),
      te.chain(commitTransactionToChainAndDecode(config)(committee)(txn)),
      te.map(addBlockchainMetadataToTransaction(txn)),
      te.chain(putTransactionAndDecode(txnsTablesName)(dynamoDB))
    );
