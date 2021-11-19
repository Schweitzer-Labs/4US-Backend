import { expect } from "chai";
import { Either, isLeft } from "fp-ts/Either";
import { ApplicationError } from "../../src/utils/application-error";
import { launchCommittee } from "../../src/clients/dapp/dapp.client";
import { genContributionRecord } from "../utils/gen-contribution.util";
import { deleteCommittee } from "../../src/utils/model/committee/delete-committee.utils";
import {
  getStratoENodeUrl,
  getStratoNodeUrl,
  getStratoOAuthClientId,
  getStratoOauthClientSecret,
  getStratoOAuthOpenIdDiscoveryUrl,
} from "../../src/utils/config";
import { initStratoConfig } from "../../src/clients/dapp/dapp.decoders";
import * as AWS from "aws-sdk";
import * as dotenv from "dotenv";
import { DynamoDB } from "aws-sdk";
import { genCommittee } from "../utils/gen-committee.util";
import stratoSQS from "../../src/strato-sqs.lambda";
import { genSQSEventWithStr } from "../utils/gen-sqs-event.util";
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

describe("Strato SQS", function () {
  let committeeRes: Either<ApplicationError, ICommittee>;
  let committee: ICommittee;
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

    const unsavedCommittee = genCommittee({
      finicityCustomerId: "5007489410",
      finicityAccountId: "5016000964",
      candidateLastName: "Schweitzer",
      candidateFirstName: "Will",
      tzDatabaseName: "America/New_York",
      state: "ny",
      scope: "local",
      party: "republican",
      race: "general",
      district: "",
      county: "saratoga",
      officeType: "supervisor",
      ruleVersion: "nyboe-2020",
      efsFilerId: 161,
    });

    committeeRes = await launchCommittee(stratoConf)(committeesTableName)(
      dynamoDB
    )(unsavedCommittee)();

    if (isLeft(committeeRes)) {
      throw Error("test failed");
    }
    committee = committeeRes.right;
  });

  it("Supports committing a transaction", async () => {
    const committeeId = committee.id;

    const txn = {
      ...genContributionRecord({ committeeId }),
      bankVerified: true,
      ruleVerified: true,
    };
    const event = genSQSEventWithStr(JSON.stringify(txn));

    const res = await stratoSQS(event, {});

    expect(res).to.equal(true);
  });

  after(async () => {
    await deleteCommittee(committeesTableName)(dynamoDB)(committee);
  });
});
