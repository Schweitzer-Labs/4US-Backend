import { expect } from "chai";
import policapitalEmailer from "../../src/policapital-emailer.lambda";
import { genValidContributionSqs } from "../events/valid-contribution.sqs";

describe("Policapital Emailer", function () {
  it("Successfully sends email on valid transaction sqs event", async () => {
    const data: any = genValidContributionSqs();
    const res = await policapitalEmailer(data, {});
    expect(res.length).to.equal(1);
  });
});
