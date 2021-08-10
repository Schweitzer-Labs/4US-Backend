import { expect } from "chai";
import transactionEventDispatcher from "../../src/transaction-event-dispatcher.lambda";
import { insertContributionEvent } from "../events/insert-contribution-event.ddb";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { genCommittee } from "../utils/gen-committee.util";
import { putCommittee } from "../../src/utils/model/put-committee.utils";
import { verifiedDispatch } from "../events/verified-dispatch.ddb";
import {
  getStratoENodeUrl,
  getStratoNodeUrl,
  getStratoOAuthClientId,
  getStratoOauthClientSecret,
  getStratoOAuthOpenIdDiscoveryUrl,
} from "../../src/utils/config";
import { initStratoConfig } from "../../src/clients/dapp/dapp.decoders";
import { launchCommittee } from "../../src/clients/dapp/dapp.client";
import { isLeft } from "fp-ts/Either";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import {
  getTxnById,
  requestTxnById,
} from "../../src/utils/model/get-txn-by-id.utils";
import { ITransaction } from "../../src/queries/search-transactions.decoder";

dotenv.config();

const committeesTableName = process.env.COMMITTEES_DDB_TABLE_NAME;
const txnTable = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const env: any = process.env.RUNENV;

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();
const ps = new AWS.SSM();

let nodeUrl: string;
let eNodeUrl: string;
let oauthClientId: string;
let oauthClientSecret: string;
let oauthOpenIdDiscoveryUrl: string;
let stratoConf: any;

let committee = genCommittee({
  district: "53",
  officeType: "senate",
  party: "democrat",
  race: "primary",
  ruleVersion: "nyboe-2020",
  scope: "state",
  state: "ny",
  tzDatabaseName: "America/New_York",
});

describe("Transaction Event Dispatch", function () {
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

    const eitherChainCommittee = await launchCommittee(stratoConf)(
      committeesTableName
    )(dynamoDB)(committee)();

    if (isLeft(eitherChainCommittee)) {
      throw Error("test failed");
    }
    committee = eitherChainCommittee.right;
  });
  it("Publishes a web contribution to a queue", async () => {
    const payload: any = insertContributionEvent(committee.id);
    const res = await transactionEventDispatcher(payload);
    expect(res.status).to.equal("success");
    expect(res.effect).to.equal("sqs_receipt_message_sent");
  });
  it("Updates ledger on reconciliation", async () => {
    const txnId = genTxnId();
    const payload: any = verifiedDispatch(committee.id)(txnId);
    const res = await transactionEventDispatcher(payload);
    const txn: any = await requestTxnById(txnTable)(dynamoDB)(committee.id)(
      txnId
    );

    expect(txn?.blockchainMetadata?.txResult?.status).to.equal("success");
  });
});
