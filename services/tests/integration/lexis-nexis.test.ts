import { expect } from "chai";
import { EntityType } from "../../src/utils/enums/entity-type.enum";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import {
  donorInputToInstantIdResult,
  IInstantIdConfig,
} from "../../src/clients/lexis-nexis/lexis-nexis.client";
import { IDonorInput } from "../../src/queries/search-donors.decoder";
import { genCommittee } from "../utils/gen-committee.util";
import * as dotenv from "dotenv";
import { genDonorInput } from "../utils/gen-donor-input.util";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";

dotenv.config();
const committee = genCommittee({});

const donorInput: IDonorInput = genDonorInput(EntityType.Ind);

const lsUsername = process.env.LN_USERNAME;
const lsPassword = process.env.LN_PASSWORD;
const billableEventsTable = process.env.BILLABLE_EVENTS_DDB_TABLE_NAME;

const instantIdConfig: IInstantIdConfig = {
  username: lsUsername,
  password: lsPassword,
};

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const dynamoDB = new DynamoDB();

describe("Lexis Nexis Instant ID", function () {
  it("Receives and decodes a valid response", async () => {
    const res: any = await pipe(
      donorInputToInstantIdResult(billableEventsTable)(dynamoDB)(
        instantIdConfig
      )(committee)(donorInput),
      taskEither.getOrElseW(() => {
        throw new Error();
      })
    )();
    expect(!!res.instantIdRawResponse).to.equal(true);
  });
});
