import { rest, util, oauthUtil, Config } from "blockapps-rest";
import { ICommittee } from "../../queries/get-committee-by-id.query";
import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../queries/search-transactions.decoder";
import { pipe } from "fp-ts/function";
import { taskEither as te } from "fp-ts";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";

const baseContract = "CommitteeContract";

import {
  decodeCreateChainResponse,
  decodeCreateUserResponse,
  getContractName,
  ICommitTransactionResponse,
  ICreateUserResponse,
  IStratoSDKConfig,
} from "./dapp.decoders";
import { putCommitteeAndDecode } from "../../utils/model/put-committee.utils";
import { putTransactionAndDecode } from "../../utils/model/put-transaction.utils";
import {
  committeeContract,
  committeeContractWithHash,
} from "./committee.contract";
import { sleep } from "../../utils/sleep.utils";
import { Options } from "blockapps-rest/src/types";

export const getClientUser = async ({ config }: IStratoSDKConfig) => {
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

export const getCommitteeHistory =
  (config: IStratoSDKConfig) => async (committee: ICommittee) => {
    const dappUser = await getClientUser(config);
    const committeeContractName = getContractName(baseContract)(committee);
    const res = await rest.search(dappUser, {
      name: `history@${committeeContractName}`,
    });
    return res;
  };

export const deployCommitteeChain =
  (sdkConfig: IStratoSDKConfig) =>
  (committee: ICommittee) =>
  async (user: ICreateUserResponse) => {
    const dappUser = await getClientUser(sdkConfig);
    const options = { config: sdkConfig.config, isDetailed: true };

    const committeeContractName = getContractName(baseContract)(committee);
    console.log(committee);

    const chainArgs = {
      label: committeeContractName,
      name: committeeContractName,
      src: committeeContractWithHash(committee.id),
      args: { _committeeId: committee.id },
      members: [
        {
          address: dappUser.address,
          enode: sdkConfig.eNodeUrl,
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
      { name: committeeContractName },
      options
    );

    console.log("chain res", chain);

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
  (sdkConfig: IStratoSDKConfig) =>
  (committee: ICommittee) =>
  (t: ITransaction) =>
  async (user: ICreateUserResponse): Promise<any> => {
    console.log("strato config", sdkConfig);

    const committeeContractName = getContractName(baseContract)(committee);

    const contract = {
      name: committeeContractName,
      address: "0000000000000000000000000000000000000100",
    };
    console.log("Committee chain id", committee.chainId);
    const options: Options = {
      config: sdkConfig.config,
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

    console.log("Blockchain metadata added to txn: ", JSON.stringify(newTxn));

    return newTxn;
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
