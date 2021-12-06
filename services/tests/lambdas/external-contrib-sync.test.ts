import { expect } from "chai";

import * as AWS from "aws-sdk";
import * as dotenv from "dotenv";
import externalContribSync from "../../src/external-contrib-sync.lambda";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

describe("Runs an External Contrib sync for all ActBlue committees", function () {
  it("Sync runs successfully", async () => {
    const res = await externalContribSync();

    expect(res).to.equal("success");
  });
});
