import { expect } from "chai";
import transactionEventDispatcher from "../../src/transaction-event-dispatcher.lambda";
import { insertContributionEvent } from "../events/insert-contribution-event.ddb";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { genCommittee } from "../utils/gen-committee.util";
import { putCommittee } from "../../src/utils/model/put-committee.utils";
dotenv.config();

const committeesTableName = process.env.COMMITTEES_DDB_TABLE_NAME;
AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

const committee = genCommittee({
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
    await putCommittee(committeesTableName)(dynamoDB)(committee);
  });
  it("Publishes a web contribution to a queue", async () => {
    const payload: any = insertContributionEvent(committee.id);
    const res = await transactionEventDispatcher(payload);
    expect(res.status).to.equal("success");
    expect(res.effect).to.equal("sqs_receipt_message_sent");
  });
});
