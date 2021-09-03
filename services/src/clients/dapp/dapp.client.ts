import { rest, oauthUtil } from "blockapps-rest";
import { Committee, ICommittee } from "../../queries/get-committee-by-id.query";
import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../queries/search-transactions.decoder";
import { pipe } from "fp-ts/function";
import { taskEither as te } from "fp-ts";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";

const baseContract = "CommitteeContract";
const txnContract = "Transaction";

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
import { committeeContractWithHash } from "./committee.contract";
import { Options } from "blockapps-rest/src/types";
import { decodeRawData } from "../../utils/decode-raw-data.util";
import { txnToMetadata } from "../../utils/txn-to-metadata.utils";

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
  (sdkConfig: IStratoSDKConfig) => async (committee: ICommittee) => {
    const dappUser = await getClientUser(sdkConfig);
    const contractName = getContractName(baseContract)(committee);
    const res = await rest.search(
      dappUser,
      {
        name: contractName,
      },
      {
        config: sdkConfig.config,
        chainIds: committee.chainId,
      }
    );
    return res;
  };

export const getTransactionHistory =
  (sdkConfig: IStratoSDKConfig) =>
  async (committee: ICommittee): Promise<any[]> => {
    const comHistory = await getCommitteeHistory(sdkConfig)(committee);
    const dappUser = await getClientUser(sdkConfig);
    const contractName = getContractName(txnContract)(committee);
    let records = [];
    if (comHistory.length > 0) {
      const txnsAddresses =
        comHistory[comHistory.length - 1]?.transactions || [];
      for (const item of txnsAddresses) {
        let res = await rest.search(
          dappUser,
          {
            name: contractName,
          },
          {
            config: sdkConfig.config,
            chainIds: [committee.chainId],
          }
        );

        records.push(res[0]);
      }
    }

    return records;
  };

export const deployCommitteeChain =
  (sdkConfig: IStratoSDKConfig) => async (committee: ICommittee) => {
    const dappUser = await getClientUser(sdkConfig);
    const options = { config: sdkConfig.config, isDetailed: true };

    const committeeContractName = getContractName(baseContract)(committee);

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

    return chain;
  };

export const initializeCommitteeChain =
  (sdkConfig: IStratoSDKConfig) =>
  async (c: ICommittee): Promise<any> => {
    const user = await getClientUser(sdkConfig);
    const committeeContractName = getContractName(baseContract)(c);

    const contract = {
      name: committeeContractName,
      address: "0000000000000000000000000000000000000100",
    };

    const options: Options = {
      config: sdkConfig.config,
      isDetailed: true,
      chainIds: [c.chainId],
    };
    const callArgs = {
      contract,
      method: "initialize",
      args: {
        _committeeId: c.id,
        _committeeName: c.committeeName,
        _state: c.state || "",
        _scope: c.scope || "",
        _officeType: c.officeType || "",
        _party: c.party || "",
        _race: c.race || "",
        _district: c.district || "",
        _county: c.county || "",
        _bankName: c.bankName || "",
        _ruleVersion: c.ruleVersion || "",
        _filerId: c.efsFilerId ? c.efsFilerId + "" : "",
        _electionId: c.efsElectionId ? c.efsElectionId + "" : "",
        _metadata: JSON.stringify({
          candidateFirstName: c.candidateFirstName,
          candidateMiddleName: c.candidateMiddleName,
          candidateLastName: c.candidateLastName,
        }),
      },
    };
    const blockchainMetadata = await rest.call(user, callArgs, options);

    const newCommittee = {
      blockchainMetadata,
      ...c,
    };

    return newCommittee;
  };

const initializeCommitteeAndDecode =
  (config: IStratoSDKConfig) =>
  (committee: ICommittee): TaskEither<ApplicationError, ICommittee> =>
    pipe(
      te.tryCatch(
        () => initializeCommitteeChain(config)(committee),
        (e) => new ApplicationError("Strato committee initialize failed", e)
      ),
      te.chain(decodeRawData("Strato init committee")(Committee))
    );

const deployCommitteeChainAndDecode =
  (config: IStratoSDKConfig) =>
  (committee: ICommittee) =>
  (user: ICreateUserResponse): TaskEither<ApplicationError, string> =>
    pipe(
      te.tryCatch(
        () => deployCommitteeChain(config)(committee),
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
  (txn: ITransaction) =>
  async (user: ICreateUserResponse): Promise<any> => {
    const committeeContractName = getContractName(baseContract)(committee);

    const contract = {
      name: committeeContractName,
      address: "0000000000000000000000000000000000000100",
    };

    const options: Options = {
      config: sdkConfig.config,
      isDetailed: true,
      chainIds: committee.chainId,
    };
    const {
      id,
      committeeId,
      direction,
      amount,
      paymentMethod,
      paymentDate,
      source,
    } = txn;

    const metadata = txnToMetadata(txn);

    const callArgs = {
      contract,
      method: "commitTransactionAndGetIndex",
      args: {
        _id: id,
        _committeeId: committeeId,
        _direction: direction,
        _amount: amount,
        _paymentMethod: paymentMethod,
        _paymentDate: paymentDate,
        _source: source,
        _metadata: JSON.stringify(metadata),
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

export const listUsers = async (config: IStratoSDKConfig) => {
  const user = await getClientUser(config);
  console.log("my address", user);
  return user;
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
      te.chain(initializeCommitteeAndDecode(config)),
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
