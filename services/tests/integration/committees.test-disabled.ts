import { expect } from "chai";
import { DynamoDB } from "aws-sdk";
import * as AWS from "aws-sdk";
import { getAllCommittees } from "../../src/repositories/committee/committee.repository";
import { isLeft } from "fp-ts/Either";

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
    // const res = await pipe(
    //   getAllCommittees("dev")(dynamoDB),
    //   taskEither.fold(
    //     () => task.of("error"),
    //     (res) => task.of("success")
    //   )
    // )();

    const res = await getAllCommittees("dev")(dynamoDB)();
    if (isLeft(res)) {
      throw res.left;
    }

    expect(res.right.length).to.equal(0);
  });
});
