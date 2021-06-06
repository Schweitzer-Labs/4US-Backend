import { expect } from "chai";
import { putCommittee } from "../../src/utils/model/put-committee.utils";
import { genCommittee } from "../utils/gen-committee.util";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import * as dotenv from "dotenv";
import { pipe } from "fp-ts/function";
import { runRulesEngine } from "../../src/pipes/rules-engine.pipe";
import { IInstantIdConfig } from "../../src/clients/lexis-nexis/lexis-nexis.client";
import { Env } from "../../src/utils/enums/env.enum";
import { task, taskEither } from "fp-ts";
import { genCreateContributionInput } from "../utils/gen-contribution.util";
import { EntityType } from "../../src/utils/enums/entity-type.enum";

dotenv.config();

const billableEventsTableName = process.env.BILLABLE_EVENTS_DDB_TABLE_NAME;
const donorsTable = process.env.DONORS_DDB_TABLE_NAME;
const txnsTable = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const rulesTable = process.env.RULES_DDB_TABLE_NAME;
const comsTable = process.env.COMMITTEES_DDB_TABLE_NAME;
const lnUsername = process.env.LN_USERNAME;
const lnPassword = process.env.LN_PASSWORD;

const instantIdConfig: IInstantIdConfig = {
  username: lnUsername,
  password: lnPassword,
};

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

const limit = 750000;
const committee = genCommittee({
  district: "53",
  officeType: "senate",
  party: "democrat",
  race: "primary",
  ruleVersion: "nyboe-2020",
  scope: "state",
  state: "ny",
});

describe("Rules engine", function () {
  before(async () => {
    await putCommittee(comsTable)(dynamoDB)(committee);
  });

  it("Disallows a contribution in excess of a committee's limit", async () => {
    const contrib = genCreateContributionInput(
      committee.id,
      750001,
      EntityType.Ind
    );

    const res = await pipe(
      runRulesEngine(billableEventsTableName)(donorsTable)(txnsTable)(
        rulesTable
      )(dynamoDB)(instantIdConfig)(committee)(contrib),
      taskEither.fold(
        (e) => {
          console.log(e);
          return task.of(e.data.remaining);
        },
        (s) => task.of("worked")
      )
    )();

    expect(res).to.equal(750000);
  });

  it("Allows a contribution within committee's limit", async () => {
    await putCommittee(comsTable)(dynamoDB)(committee);

    const contrib = genCreateContributionInput(
      committee.id,
      750000,
      EntityType.Ind
    );

    const res = await pipe(
      runRulesEngine(billableEventsTableName)(donorsTable)(txnsTable)(
        rulesTable
      )(dynamoDB)(instantIdConfig)(committee)(contrib),
      taskEither.fold(
        (e) => task.of(e.data.remaining),
        (res) => {
          return task.of(res.balance);
        }
      )
    )();

    expect(res).to.equal(0);
  });
});
