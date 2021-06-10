import { expect } from "chai";
import platformContribute from "../../src/platform-contribute.lambda";
import { EmploymentStatus } from "../../src/utils/enums/employment-status";

interface GenPlatformContribConfig {
  committeeId: string;
  amount: number;
  firstName?: string;
  lastName?: string;
  addressLine1?: string;
  city?: string;
  state?: string;
  postalCode?: string;
  entityType?: string;
  emailAddress?: string;
  entityName?: string;
  employer?: string;
  employmentStatus?: string;
  occupation?: string;
  middleName?: string;
  refCode?: string;
  addressLine2?: string;
  phoneNumber?: string;
  attestsToBeingAnAdultCitizen?: boolean;
  cardNumber?: string;
}

const genPlatformContribution = (config: GenPlatformContribConfig) => ({
  firstName: "Evan",
  lastName: "Piro",
  addressLine1: "123 street",
  city: "downingtown",
  state: "pa",
  postalCode: "11324",
  employmentStatus: EmploymentStatus.SelfEmployed,
  employer: "hello",
  entityType: "Ind",
  cardNumber: config.cardNumber || "4242424242424242",
  cardExpirationMonth: 12,
  cardExpirationYear: 2023,
  cardCVC: "123",
  phoneNumber: "1231231234",
  attestsToBeingAnAdultCitizen: true,
  ...config,
});

const genEvent = (payload: object) => {
  return {
    body: JSON.stringify(payload),
  };
};

const contrib = genPlatformContribution({
  committeeId: "john-safford",
  amount: 2000,
});

const testEvent = genEvent(contrib);

describe("Platform Contribute", function () {
  it("Accepts a valid contribution", async () => {
    const contrib = genPlatformContribution({
      committeeId: "john-safford",
      amount: 100,
    });

    const testEvent = genEvent(contrib);

    const res = await platformContribute(testEvent);
    const body = JSON.parse(res.body);

    expect(res.statusCode).to.equal(200);
    expect(body.message).to.equal("success");
  });

  it("Reject an excess contribution", async () => {
    const contrib = genPlatformContribution({
      committeeId: "john-safford",
      amount: 480100,
    });

    const testEvent = genEvent(contrib);

    const res = await platformContribute(testEvent);
    const body = JSON.parse(res.body);

    expect(res.statusCode).to.equal(401);
    expect(body.message).to.equal("Excess contribution attempted");
    expect(body.remaining).to.be.a("number");
  });

  it("Reject bad card info", async () => {
    const contrib = genPlatformContribution({
      committeeId: "john-safford",
      amount: 100,
      cardNumber: "4242424241414141",
    });

    const testEvent = genEvent(contrib);

    const res = await platformContribute(testEvent);
    const body = JSON.parse(res.body);

    expect(res.statusCode).to.equal(422);
    expect(body.message).to.equal(
      "Payment failed. Please ensure your card info is correct."
    );
  });
});
