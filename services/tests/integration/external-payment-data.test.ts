import { expect } from "chai";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

// Onboard a committee
// Link external payments data provider
// Sync external payments

// Given a customer, I can onboard them as a committee and sync them
// with an external data provider.

// Given a committee and a configured external payments provider, I can run a sync
// with the external payments provider such that the external data
// will appear as unreconciled transactions within the transactions feed
//

describe("Describe here", function () {
  it("Tests something", async () => {
    expect(true).to.equal(false);
  });
});
