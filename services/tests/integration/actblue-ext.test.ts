import { expect } from "chai";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

describe("ActBlue to External Transaction Synchronization", function () {
  it("Tests something", async () => {
    expect(true).to.equal(false);
  });
});
