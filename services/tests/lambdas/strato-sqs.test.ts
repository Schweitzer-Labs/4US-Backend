import { expect } from "chai";
import stratoSQS from "../../src/strato-sqs.lambda";
import { validContributionSqs } from "../events/valid-contribution.sqs";

describe("Strato SQS", function () {
  it("Commits a transaction to committee contract", async () => {
    const data: any = validContributionSqs;
    const res = await stratoSQS(data, {});
    expect(res.length).to.equal(1);
  });
});
