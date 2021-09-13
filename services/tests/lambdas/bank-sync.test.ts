import { expect } from "chai";

import * as AWS from "aws-sdk";
import * as dotenv from "dotenv";
import bankSync from "../../src/bank-sync.lambda";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

describe("Syncs transactions with a platform account", function () {
  it("Sync runs successfully", async () => {
    const res = await bankSync();

    expect(res).to.equal("success");
  });
});
