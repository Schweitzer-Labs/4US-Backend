import { expect } from "chai";
import { committeeIdToDDBRes } from "../../src/utils/model/committee/get-committee-by-id.query";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";

let committeesTableName: string;
let committeeId: string;
let dynamoDB: DynamoDB;

describe("Get Committee by ID Query", function () {
  before(function () {
    committeesTableName = "committees-dev";
    committeeId = "24786b4b-df27-495e-ad8a-b7a65f31370a";
    AWS.config.apiVersions = {
      dynamodb: "2012-08-10",
    };
    dynamoDB = new DynamoDB();
  });
  it("Retrieves a committee by valid ID", async () => {
    const res = await committeeIdToDDBRes(committeesTableName)(dynamoDB)(
      committeeId
    );
    expect(res).to.equal(false);
  });
});
