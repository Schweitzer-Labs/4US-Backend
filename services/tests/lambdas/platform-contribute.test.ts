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
  cardNumber: "4242424242424242",
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
  amount: 20000,
});

const testEvent = genEvent(contrib);

describe("Platform Contribute", function () {
  it("Accepts a valid contribution", async () => {
    const res = await platformContribute(testEvent);
    console.log(res);

    expect(res.body).to.equal("success");
  });
});
