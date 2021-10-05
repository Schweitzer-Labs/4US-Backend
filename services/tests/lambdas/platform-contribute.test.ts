import { expect } from "chai";
import platformContribute from "../../src/platform-contribute.lambda";
import { EmploymentStatus } from "../../src/utils/enums/employment-status";
import * as faker from "faker";
import { genCommittee } from "../utils/gen-committee.util";
import { putCommittee } from "../../src/utils/model/put-committee.utils";
import { sleep } from "../../src/utils/sleep.utils";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { deleteCommittee } from "../../src/utils/model/delete-committee.utils";
import { EntityType } from "../../src/utils/enums/entity-type.enum";
import { IOwner } from "../../src/queries/search-transactions.decoder";
import { genOwners } from "../utils/gen-owners.util";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;

interface GenPlatformContribConfig {
  committeeId?: string;
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
  owners?: IOwner[];
}

const committee = genCommittee({
  state: "ny",
  scope: "local",
  party: "republican",
  race: "general",
  district: "",
  county: "saratoga",
  officeType: "supervisor",
  ruleVersion: "nyboe-2020",
});

const genPlatformContribution = (config: GenPlatformContribConfig) => ({
  committeeId: committee.id,
  firstName: faker.name.firstName(),
  lastName: faker.name.lastName(),
  addressLine1: faker.address.streetAddress(),
  city: "downingtown",
  state: "PA",
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

describe("Platform Contribute", function () {
  before(async () => {
    await putCommittee(committeesTableName)(dynamoDB)(committee);
    await sleep(1000);
  });
  it("Accepts a valid contribution", async () => {
    const contrib = genPlatformContribution({
      committeeId: committee.id,
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
      committeeId: committee.id,
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
      committeeId: committee.id,
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

  it("Accepts a family contribution", async () => {
    const req = genPlatformContribution({
      amount: 2500,
      entityType: "Fam",
      attestsToBeingAnAdultCitizen: true,
    });

    const event = genEvent(req);

    const res = await platformContribute(event);

    expect(res.statusCode).to.equal(200);
  });

  it("Accepts an LLC contribution", async () => {
    const req = genPlatformContribution({
      committeeId: "will-schweitzer",
      amount: 51,
      entityType: EntityType.Llc,
      entityName: faker.company.companyName(),
      owners: genOwners(),
    });

    const event = genEvent(req);

    const res = await platformContribute(event);

    expect(res.statusCode).to.equal(200);
  });

  it("Accepts a union contribution", async () => {
    const req = genPlatformContribution({
      committeeId: "will-schweitzer",
      amount: 51,
      entityType: "Union",
      entityName: faker.company.companyName(),
    });

    const event = genEvent(req);

    const res = await platformContribute(event);

    const body = JSON.stringify(res.body);

    expect(res.statusCode).to.equal(200);
  });

  describe("Payment Processing Only Contributions", function () {
    it("Bypasses rules engine", async () => {
      const req = {
        committeeId: "ian-cain",
        amount: 2500,
        firstName: "Evan",
        lastName: "Piro",
        addressLine1: "1364 asdfsadf",
        city: "adsfadsf",
        state: "CA",
        postalCode: "13224",
        entityType: "Ind",
        emailAddress: "dev.evanpiro@gmail.com",
        cardNumber: "4242424242424242",
        cardExpirationMonth: 12,
        cardExpirationYear: 2023,
        cardCVC: "123",
        employmentStatus: "Unemployed",
        attestsToBeingAnAdultCitizen: true,
      };

      const event = genEvent(req);

      const res = await platformContribute(event);

      expect(res.statusCode).to.equal(200);
    });
  });
  after(async () => {
    await deleteCommittee(committeesTableName)(dynamoDB)(committee);
  });
});
