import { genDonorInput } from "../utils/gen-donor-input.util";
import { EntityType } from "../../src/utils/enums/entity-type.enum";
import { verifyDonor } from "../../src/pipes/donor-verification.pipe";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { expect } from "chai";
import { IInstantIdConfig } from "../../src/clients/lexis-nexis/lexis-nexis.client";
import { genCommittee } from "../utils/gen-committee.util";

dotenv.config();

const donorsTableName = process.env.DONORS_DDB_TABLE_NAME;
const billableEventsTableName = process.env.BILLABLE_EVENTS_DDB_TABLE_NAME;

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();
const lnUsername = process.env.LN_USERNAME;
const lnPassword = process.env.LN_PASSWORD;

const instantIdConfig: IInstantIdConfig = {
  username: lnUsername,
  password: lnPassword,
};

const committee = genCommittee({});

describe("Donor Verifier", function () {
  it("Creates a verified individual donor record with new donor input", async () => {
    const donorInput = genDonorInput(EntityType.IND);
    const res: any = await pipe(
      verifyDonor(billableEventsTableName)(donorsTableName)(dynamoDB)(
        instantIdConfig
      )(committee)(donorInput),
      taskEither.getOrElseW(() => {
        throw new Error();
      })
    )();

    expect(!!res.instantIdRawResponse).to.equal(true);
  });

  it("Matches an existing individual donor with recognized donor input", async () => {
    const donorInput = genDonorInput(EntityType.IND);
    const res1: any = await pipe(
      verifyDonor(billableEventsTableName)(donorsTableName)(dynamoDB)(
        instantIdConfig
      )(committee)(donorInput),
      taskEither.getOrElseW(() => {
        throw new Error();
      })
    )();

    const res2: any = await pipe(
      verifyDonor(billableEventsTableName)(donorsTableName)(dynamoDB)(
        instantIdConfig
      )(committee)(donorInput),
      taskEither.getOrElseW(() => {
        throw new Error();
      })
    )();

    expect(res1.id === res2.id).to.equal(true);
  });

  it("Matches an existing non-individual donor with recognized donor input", async () => {
    const donorInput = genDonorInput(EntityType.LLC);
    const res1: any = await pipe(
      verifyDonor(billableEventsTableName)(donorsTableName)(dynamoDB)(
        instantIdConfig
      )(committee)(donorInput),
      taskEither.getOrElseW(() => {
        throw new Error();
      })
    )();

    const res2: any = await pipe(
      verifyDonor(billableEventsTableName)(donorsTableName)(dynamoDB)(
        instantIdConfig
      )(committee)(donorInput),
      taskEither.getOrElseW(() => {
        throw new Error();
      })
    )();

    expect(res1.id === res2.id).to.equal(true);
  });
});
