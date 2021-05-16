import { expect } from "chai";
import { DynamoDB } from "aws-sdk";
import * as AWS from "aws-sdk";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";
import { getAllCommittees } from "../../src/repositories/committee/committee.repository";

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
      getAllCommittees("dev")(dynamoDB),
      taskEither.fold(
        () => task.of("error"),
        (res) => task.of("success")
      )
    )();

    expect(res).to.equal("success");
  });
});
