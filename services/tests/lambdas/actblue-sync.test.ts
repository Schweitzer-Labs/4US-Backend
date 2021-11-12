import { expect } from "chai";

import * as AWS from "aws-sdk";
import * as dotenv from "dotenv";
import actBlueSync from "../../src/actblue-sync.lambda";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

describe("Runs an ActBlue sync for all ActBlue committees", function () {
  it("Sync runs successfully", async () => {
    const res = await actBlueSync();

    expect(res).to.equal("success");
  });
});
