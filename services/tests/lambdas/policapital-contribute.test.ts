import { expect } from "chai";
import contribute from "../../src/policapital-contribute.lambda";
import { invalidPolicapitalContributeProxy } from "../events/invalid-policapital-contribute.proxy";
import { validPolicapitalContributeProxy } from "../events/valid-policapital-contribute-proxy";
import { seedCommittees } from "../seed/seed-committees.job";
import { genCommittee } from "../utils/gen-committee.util";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { putCommittee } from "../../src/utils/model/put-committee.utils";
import * as dotenv from "dotenv";
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
});

describe("Policapital Contribute Lambda", function () {
  before(async () => {
    await putCommittee(committeesTableName)(dynamoDB)(committee);
  });

  it("Stops a contribution call with an invalid payload", async () => {
    const result = await contribute(invalidPolicapitalContributeProxy);
    expect(result.statusCode).to.equal(400);

    let response = JSON.parse(result.body);
    expect(response).to.be.an("object");
    expect(response.message).to.be.an("string");
  });

  it("Allows a contribution call with a valid payload", async () => {
    const result = await contribute(
      validPolicapitalContributeProxy(committee.id)
    );
    expect(result.statusCode).to.equal(200);

    let response = JSON.parse(result.body);
    expect(response).to.be.an("object");
    expect(response.message).to.be.equal("success");
  });
});
