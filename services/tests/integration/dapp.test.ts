import { expect } from "chai";

import { genCommittee } from "../utils/gen-committee.util";
import { putCommittee } from "../../src/utils/model/put-committee.utils";
import * as AWS from "aws-sdk";
import * as dotenv from "dotenv";
import { DynamoDB } from "aws-sdk";
import {
  initStratoConfig,
  IStratoSDKConfig,
} from "../../src/clients/dapp/dapp.decoders";
import {
  commitTransaction,
  getCommitteeHistory,
  launchCommittee,
} from "../../src/clients/dapp/dapp.client";
import { genContributionRecord } from "../utils/gen-contribution.util";
import { isLeft } from "fp-ts/Either";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import {
  getStratoENodeUrl,
  getStratoNodeUrl,
  getStratoOAuthClientId,
  getStratoOauthClientSecret,
  getStratoOAuthOpenIdDiscoveryUrl,
} from "../../src/utils/config";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const env: any = process.env.RUNENV;

// const eNodeUrl =
//   "enode://042cf697543c8b1d1f64e531af83f5f78d2d0d9bb77e42c5d366092b13eec4ed6f418c5a1036feccfc9b234ce1d7dc4edbb22516dfa37cf269ae11e3dac0394112@54.80.236.98:30303";
//
// // // Dev
// const config: IStratoSDKConfig = {
//   apiDebug: false,
//   timeout: 600000,
//   nodes: [
//     {
//       id: 0,
//       url: "https://schweitzerlabs-dev1.partnernet.blockapps.net:8080",
//       oauth: {
//         clientId: "dev",
//         clientSecret: "cc89585a-8841-43ee-814e-97d217d4c016",
//         openIdDiscoveryUrl:
//           "https://keycloak.blockapps.net/auth/realms/hosting-schweitzer-labs/.well-known/openid-configuration",
//       },
//     },
//   ],
//   eNodeUrl,
// };

let nodeUrl: string;
let eNodeUrl: string;
let oauthClientId: string;
let oauthClientSecret: string;
let oauthOpenIdDiscoveryUrl: string;
let stratoConf: any;

const ps = new AWS.SSM();

//
// const config: IStratoSDKConfig = initStratoConfig({
//   nodeId: number;
//   nodeUrl: string;
//   eNodeUrl: string;
//   port: number;
//   oauthClientId: string;
//   oauthClientSecret: string;
//   oauthOpenIdDiscoveryUrl: string;
// })

// Prod
// const config: IStratoSDKConfig = {
//   apiDebug: false,
//   timeout: 600000,
//   nodes: [
//     {
//       id: 0,
//       url: "https://schweitzerlabs1.hosting.blockapps.net:8080",
//       port: 8080,
//       oauth: {
//         clientId: "27qebqot4nmest9f0pen8q8v5i",
//         clientSecret: "77hes2rvk922unfcnfe7bettjd00sphrl1vt0490dfp7qupvv9u",
//         scope: "https://4us/strato/dummy",
//         openIdDiscoveryUrl:
//           "https://cognito-idp.us-east-1.amazonaws.com/us-east-1_xEOZcvQ5t/.well-known/openid-configuration",
//       },
//     },
//   ],
// };

const genNYCommittee = () =>
  genCommittee({
    district: "53",
    officeType: "senate",
    party: "democrat",
    race: "primary",
    ruleVersion: "nyboe-2020",
    scope: "state",
    state: "ny",
    tzDatabaseName: "America/New_York",
  });

describe("DAPP Tests", async () => {
  before(async () => {
    if (
      !nodeUrl ||
      !eNodeUrl ||
      !oauthClientId ||
      !oauthClientSecret ||
      !oauthOpenIdDiscoveryUrl
    ) {
      nodeUrl = await getStratoNodeUrl(ps)(env);
      eNodeUrl = await getStratoENodeUrl(ps)(env);
      oauthClientId = await getStratoOAuthClientId(ps)(env);
      oauthClientSecret = await getStratoOauthClientSecret(ps)(env);
      oauthOpenIdDiscoveryUrl = await getStratoOAuthOpenIdDiscoveryUrl(ps)(env);
    }

    stratoConf = initStratoConfig({
      nodeUrl,
      eNodeUrl,
      oauthClientId,
      oauthClientSecret,
      oauthOpenIdDiscoveryUrl,
    });
  });
  // describe("Initialize app user", async () => {
  //   it("Initializes dapp user with client credentials", async () => {
  //     const res = await getClientUser(config);
  //     expect(res.token).to.be.a("string");
  //   });
  // });
  describe("Committee Contract", async () => {
    it("Assigns a committee a private chain", async () => {
      console.log("strato conf", stratoConf);
      const committee = genNYCommittee();

      const eitherChainCommittee = await launchCommittee(stratoConf)(
        committeesTableName
      )(dynamoDB)(committee)();

      if (isLeft(eitherChainCommittee)) {
        throw Error("test failed");
      }

      console.log("test res here", eitherChainCommittee);

      // const history = await getCommitteeHistory(stratoConf)(committee);
      // console.log("com hist", history);

      expect(eitherChainCommittee.right.chainId).to.be.a("string");
    });
    it("Supports committing a transaction", async () => {
      const committee = genNYCommittee();
      const txn = genContributionRecord(committee.id);

      const res = await pipe(
        launchCommittee(stratoConf)(committeesTableName)(dynamoDB)(committee),
        taskEither.chain((committeeWithChain) =>
          commitTransaction(stratoConf)(txnsTableName)(dynamoDB)(
            committeeWithChain
          )(txn)
        )
      )();

      if (isLeft(res)) {
        throw Error("test failed");
      }

      expect(res.right.blockchainMetadata).to.be.a("object");
    });
    // it("Supports getting a transaction by index number", async () => {
    //   expect(false).to.equal(true);
    // });

    // it("Supports a basic method call", async () => {
    //   const committee = genNYCommittee();
    //
    //   const res = await pipe(
    //     launchCommittee(eNodeURL)(config)(committeesTableName)(dynamoDB)(
    //       committee
    //     ),
    //     taskEither.chain((committeeWithChainId) =>
    //       pipe(
    //         getClientUserAndDecode(config),
    //         taskEither.chain((user) =>
    //           taskEither.tryCatch(
    //             () => callMethodOnContract(config)(committeeWithChainId)(user),
    //             (e) => new ApplicationError("failed", e)
    //           )
    //         )
    //       )
    //     )
    //   )();
    //
    //   if (isLeft(res)) {
    //     throw res.left;
    //   }
    //   console.log("evan response", res.right);
    //   expect(false).to.equal(true);
    // });
  });
});
