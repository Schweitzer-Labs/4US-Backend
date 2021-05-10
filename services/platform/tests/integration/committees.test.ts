import { expect } from "chai";
import { getAllCommittees } from "../../src/clients/committees/committees.client";
import { DynamoDB } from "aws-sdk";
import * as AWS from "aws-sdk";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";

let dynamoDB: DynamoDB;
describe("Committee Store", function () {
  before(() => {
    AWS.config.apiVersions = {
      dynamodb: "2012-08-10",
    };
    AWS.config.update({ region: "us-east-1" });
    dynamoDB = new DynamoDB();
  });
  it("Queries committee table", async () => {
    const res = await pipe(
      getAllCommittees(dynamoDB),
      taskEither.fold(
        () => task.of([]),
        (res) => task.of(res)
      )
    )();

    expect(res[0].committeeName).to.equal("Test Candidate");
  });
});
