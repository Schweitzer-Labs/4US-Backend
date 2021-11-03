import { expect } from "chai";
import bluelink from "../../src/bluelink.lambda";
import { genCommittee } from "../utils/gen-committee.util";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const genEvent = (payload: object) => {
  return {
    headers: {
      ["4US-App-Key"]: "0e6fa92b-0ab1-4d38-a521-67579010abaa",
    },
    body: JSON.stringify(payload),
  };
};

const genEventWithoutAuth = (payload: object) => {
  return {
    body: JSON.stringify(payload),
  };
};

describe("Bluelink webhook", function () {
  it("Returns a 401 on auth failure", async () => {
    const testEvent = genEventWithoutAuth({});

    const res = await bluelink(testEvent);
    const body = JSON.parse(res.body);

    expect(res.statusCode).to.equal(401);
    expect(body.message).to.equal(
      `Invalid auth. Please ensure headers contain property "4US-App-Key"`
    );
  });

  it("Returns a 400 on schema failure", async () => {
    const testEvent = genEvent({
      amount: "123",
      firstName: 100,
      paymentDate: 123,
    });

    const res = await bluelink(testEvent);
    const body = JSON.parse(res.body);

    expect(res.statusCode).to.equal(400);
    expect(body.message).to.equal(`"recipientId" is required`);
  });

  it("Returns a 200 on schema success", async () => {
    const testEvent = genEvent({
      recipientId: "testeststest",
      paymentDate: 1231232323,
      amount: 123,
      firstName: "Test",
      middleName: "Test",
      lastName: "Test",
      addressLine1: "Test",
      city: "Test",
      state: "PA",
      country: "Test",
      postalCode: "13224",
      emailAddress: "dev.evanpiro@gmail.com",
      employer: "Test",
      employmentStatus: "Employed",
      occupation: "Test",
      refCode: "Test",
      addressLine2: "Test",
      phoneNumber: "6103161022",
      metadata: {
        dataOne: "asdfasdf",
        dataTwo: "asdfasdf",
      },
    });

    const res = await bluelink(testEvent);
    const body = JSON.parse(res.body);

    console.log(body.message);

    expect(res.statusCode).to.equal(200);
    expect(body.message).to.equal("success");
  });

  after(async () => {
    // await deleteCommittee(committeesTableName)(dynamoDB)(committee);
  });
});
