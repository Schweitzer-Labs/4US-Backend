import { genDonorInput } from "../utils/gen-donor-input.util";
import { EntityType } from "../../src/utils/enums/entity-type.enum";
import { verifyDonor } from "../../src/pipes/donor-verification.pipe";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { Env } from "../../src/utils/enums/env.enum";
import { taskEither } from "fp-ts";
import { expect } from "chai";

dotenv.config();

const donorsTableName = process.env.DONORS_DDB_TABLE_NAME;
AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

const iidConfig = {
  username: "fake_user",
  password: "fake_password",
  env: Env.Dev,
};

describe("Donor Verifier", function () {
  it("Creates a verified individual donor record with new donor input", async () => {
    const donorInput = genDonorInput(EntityType.IND);
    const res: any = await pipe(
      verifyDonor(donorsTableName)(dynamoDB)(iidConfig)(donorInput),
      taskEither.getOrElseW(() => {
        throw new Error();
      })
    )();

    expect(res.instantIdComprehensiveVerificationScore).to.be.a("number");
  });

  it("Matches an existing individual donor with recognized donor input", async () => {
    const donorInput = genDonorInput(EntityType.IND);
    const res1: any = await pipe(
      verifyDonor(donorsTableName)(dynamoDB)(iidConfig)(donorInput),
      taskEither.getOrElseW(() => {
        throw new Error();
      })
    )();

    const res2: any = await pipe(
      verifyDonor(donorsTableName)(dynamoDB)(iidConfig)(donorInput),
      taskEither.getOrElseW(() => {
        throw new Error();
      })
    )();

    expect(res1.id === res2.id).to.equal(true);
  });

  it("Matches an existing non-individual donor with recognized donor input", async () => {
    const donorInput = genDonorInput(EntityType.LLC);
    const res1: any = await pipe(
      verifyDonor(donorsTableName)(dynamoDB)(iidConfig)(donorInput),
      taskEither.getOrElseW(() => {
        throw new Error();
      })
    )();

    const res2: any = await pipe(
      verifyDonor(donorsTableName)(dynamoDB)(iidConfig)(donorInput),
      taskEither.getOrElseW(() => {
        throw new Error();
      })
    )();

    expect(res1.id === res2.id).to.equal(true);
  });
});
