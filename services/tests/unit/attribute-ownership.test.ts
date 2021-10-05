import { expect } from "chai";
import { genCommittee } from "../utils/gen-committee.util";
import { toAttributedContribs } from "../../src/pipes/generate-disclosure.pipe";
import { genOwners, genTxnWithOwners } from "../utils/gen-owners.util";

const committee = genCommittee({ scope: "local" });

describe("Ownership attribution", function () {
  it("Properly attributes ownership", () => {
    for (let i = 0; i < 100; i++) {
      console.log("\n", "------------------------------", "\n");
      const owners = genOwners();
      const txn = genTxnWithOwners(owners);

      const res = toAttributedContribs(committee)(txn);

      console.log(
        "owner percentages",
        owners.map((val) => val.percentOwnership)
      );

      console.log("txn total", txn.amount);

      const totalAmount = res.reduce((acc, val) => {
        console.log("before parse", val["ORG_AMT"]);
        const amount = Math.round(parseFloat(val["ORG_AMT"]) * 100);
        console.log("parsed amount", amount);

        return amount + acc;
      }, 0);

      console.log("number of owners tested", i);
      expect(totalAmount).to.equal(txn.amount);
    }
  });
});
