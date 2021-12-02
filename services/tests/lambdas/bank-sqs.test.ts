import { expect } from "chai";

import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import * as dotenv from "dotenv";
import { genCommittee } from "../utils/gen-committee.util";
import { deleteCommittee } from "../../src/utils/model/committee/delete-committee.utils";
import bankSqs from "../../src/bank-sqs.lambda";
import { putCommittee } from "../../src/utils/model/committee/put-committee.utils";
import { SQSEvent } from "aws-lambda";
import { genSQSEventWithStr } from "../utils/gen-sqs-event.util";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const dynamoDB = new DynamoDB();

const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;

const committee = genCommittee({
  district: "53",
  officeType: "senate",
  party: "democrat",
  race: "primary",
  ruleVersion: "nyboe-2020",
  scope: "state",
  state: "ny",
  tzDatabaseName: "America/New_York",
  finicityCustomerId: "5007489410",
  finicityAccountId: "5016000964",
});

const sqsEvent: SQSEvent = genSQSEventWithStr(committee.id);

describe("Processes a committee's transactions", function () {
  before(async () => {
    await putCommittee(committeesTableName)(dynamoDB)(committee);
  });
  it("Sync runs successfully", async () => {
    const res = await bankSqs(sqsEvent);

    expect(res).to.equal(true);
  });

  after(async () => {
    await deleteCommittee(committeesTableName)(dynamoDB)(committee);
  });
});
