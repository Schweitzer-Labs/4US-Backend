import { genDonor } from "../utils/gen-donor.util";
import { EntityType } from "../../src/utils/enums/entity-type.enum";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { committeeAndDonorToRule } from "../../src/queries/get-rule.query";
import { getOrElseW } from "fp-ts/TaskEither";
import { expect } from "chai";
import * as dotenv from "dotenv";
import { ICommittee } from "../../src/types/committee.type";

dotenv.config();

const hash = "[ny][state][democrat][general][35][][assembly][nyboe-2020][ind]";

const committee: ICommittee = {
  id: "123",
  committeeName: "Taylor for Office",
  candidateFirstName: "Taylor",
  candidateLastName: "Jacobs",
  stripeAccount: "123123",
  members: ["sdf"],
  tzDatabaseName: "est",
  platformPlan: "4US",
  state: "ny",
  scope: "state",
  party: "democrat",
  race: "general",
  district: "35",
  county: "",
  officeType: "assembly",
  ruleVersion: "nyboe-2020",
};

const limit = 470000;

const rulesTableName = process.env.RULES_DDB_TABLE_NAME;
AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const dynamoDB = new DynamoDB();

describe("Get Rule Query", function () {
  it("Returns a rule for a given committee and donor", async () => {
    const donor = genDonor(EntityType.Ind);
    const res: any = await pipe(
      committeeAndDonorToRule(rulesTableName)(dynamoDB)(committee)(donor),
      getOrElseW(() => {
        throw new Error();
      })
    )();

    expect(res.limit).to.equal(limit);
  });
});
