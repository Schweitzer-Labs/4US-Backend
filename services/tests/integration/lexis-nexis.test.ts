import { expect } from "chai";
import { Env } from "../../src/utils/enums/env.enum";
import { EntityType } from "../../src/utils/enums/entity-type.enum";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { donorInputToInstantIdResult } from "../../src/clients/lexis-nexis/lexis-nexis.client";
import { IDonorInput } from "../../src/queries/search-donors.decoder";

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
      donorInputToInstantIdResult({
        env: Env.Dev,
        username: "fake_name",
        password: "fake_password",
      })(donorInput),
      taskEither.getOrElseW(() => {
        throw new Error();
      })
    )();
    expect(res.instantIdComprehensiveVerificationScore).to.equal(20);
  });
});
