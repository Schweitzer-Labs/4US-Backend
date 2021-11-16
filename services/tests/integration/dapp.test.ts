import { expect } from "chai";

import { genCommittee } from "../utils/gen-committee.util";
import * as AWS from "aws-sdk";
import * as dotenv from "dotenv";
import { DynamoDB } from "aws-sdk";
import { initStratoConfig } from "../../src/clients/dapp/dapp.decoders";
import {
  commitTransaction,
  getTransactionHistory,
  launchCommittee,
} from "../../src/clients/dapp/dapp.client";
import { genContributionRecord } from "../utils/gen-contribution.util";
import { Either, isLeft } from "fp-ts/Either";
import {
  getStratoENodeUrl,
  getStratoNodeUrl,
  getStratoOAuthClientId,
  getStratoOauthClientSecret,
  getStratoOAuthOpenIdDiscoveryUrl,
} from "../../src/utils/config";
import { sleep } from "../../src/utils/sleep.utils";
import { deleteCommittee } from "../../src/utils/model/committee/delete-committee.utils";
import { ApplicationError } from "../../src/utils/application-error";
import { ICommittee } from "../../src/model/committee.type";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const env: any = process.env.RUNENV;

let nodeUrl: string;
let eNodeUrl: string;
let oauthClientId: string;
let oauthClientSecret: string;
let oauthOpenIdDiscoveryUrl: string;
let stratoConf: any;

const ps = new AWS.SSM();

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
  describe("Committee Contract", async () => {
    describe("Launch committee", function () {
      let committeeRes: Either<ApplicationError, ICommittee>;
      let committee: ICommittee;

      before(async () => {
        committeeRes = await launchCommittee(stratoConf)(committeesTableName)(
          dynamoDB
        )(genNYCommittee())();

        if (isLeft(committeeRes)) {
          throw Error("test failed");
        }
        committee = committeeRes.right;
      });

      it("Assigns a committee a private chain", async () => {
        expect(committee.chainId).to.be.a("string");
        expect(committee.blockchainMetadata["status"]).to.equal("Success");
      });
      after(async () => {
        await deleteCommittee(committeesTableName)(dynamoDB)(committee);
      });
    });
    describe("Commit Transactions", function () {
      let committeeRes: Either<ApplicationError, ICommittee>;
      let committee: ICommittee;
      before(async () => {
        committeeRes = await launchCommittee(stratoConf)(committeesTableName)(
          dynamoDB
        )(genNYCommittee())();

        if (isLeft(committeeRes)) {
          throw Error("test failed");
        }
        committee = committeeRes.right;
      });

      it("Supports committing a transaction", async () => {
        const committeeId = committee.id
        const txn = genContributionRecord({committeeId});

        const res = await commitTransaction(stratoConf)(txnsTableName)(
          dynamoDB
        )(committee)(txn)();

        if (isLeft(res)) {
          throw Error("test failed");
        }

        await sleep(3000);

        const txnHis = await getTransactionHistory(stratoConf)(committee);
        expect(res.right.blockchainMetadata["status"]).to.equal("Success");
        expect(txnHis[0]?.index).to.equal(0);
      });

      after(async () => {
        await deleteCommittee(committeesTableName)(dynamoDB)(committee);
      });
    });
  });
});
