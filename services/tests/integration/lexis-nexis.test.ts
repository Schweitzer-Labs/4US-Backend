import { expect } from "chai";
import { donorInputToInstanceIdResult } from "../../src/clients/lexis-nexis/lexis-nexis.client";
import { Env } from "../../src/utils/enums/env.enum";
import { EntityType } from "../../src/utils/enums/entity-type.enum";
import { IDonorInput } from "../../src/queries/search-donors.query";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { isLeft } from "fp-ts/Either";

const donorInput: IDonorInput = {
  firstName: "Test",
  lastName: "Test",
  addressLine1: "Test",
  city: "Test",
  state: "Test",
  postalCode: "Test",
  entityType: EntityType.IND,
  id: "test",
  emailAddress: "test",
  middleName: "test",
  addressLine2: "test",
  employer: "test",
  occupation: "test",
  companyName: "test",
  phoneNumber: "test",
  attestsToBeingAnAdultCitizen: true,
  cardNumberLastFourDigits: "test",
  entityName: "test",
};

describe("Lexis Nexis Instant ID", function () {
  it("Receives and decodes a valid response", async () => {
    const res: any = await pipe(
      donorInputToInstanceIdResult({
        env: Env.Dev,
        username: "fake_name",
        password: "fake_password",
      })(donorInput),
      taskEither.getOrElseW(() => {
        throw new Error();
      })
    )();
    console.log(res);

    expect(res.comprehensiveVerificationScore).to.equal(20);
  });
});
