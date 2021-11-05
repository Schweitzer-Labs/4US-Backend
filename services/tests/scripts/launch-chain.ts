import * as AWS from "aws-sdk";
import * as dotenv from "dotenv";
import { DynamoDB } from "aws-sdk";

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
import { initStratoConfig } from "../../src/clients/dapp/dapp.decoders";
import { launchCommittee } from "../../src/clients/dapp/dapp.client";
import { getCommitteeById } from "../../src/utils/model/get-committee-by-id.query";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

AWS.config.region = "us-east-1";
const dynamoDB = new DynamoDB();

// const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
// const env: any = process.env.RUNENV;

const committeesTableName: any = "prod-4us-backend-Committees-1HO9F0CX9Y3VL";
const env: any = "prod";

let nodeUrl: string;
let eNodeUrl: string;
let oauthClientId: string;
let oauthClientSecret: string;
let oauthOpenIdDiscoveryUrl: string;
let stratoConf: any;

const ps = new AWS.SSM();

const run = async () => {
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

  const res = await pipe(
    getCommitteeById(committeesTableName)(dynamoDB)("john-safford"),
    taskEither.chain(launchCommittee(stratoConf)(committeesTableName)(dynamoDB))
  )();

  if (isLeft(res)) {
    throw res.left;
  }
  console.log(res.right.chainId);

  // const history = await getCommitteeHistory(stratoConf)(res.right);
  // console.log("com hist", history);
};

run();
