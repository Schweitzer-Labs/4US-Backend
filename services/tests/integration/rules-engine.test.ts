import { expect } from "chai";
import { putCommittee } from "../../src/utils/model/committee/put-committee.utils";
import { genCommittee } from "../utils/gen-committee.util";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import * as dotenv from "dotenv";
import { pipe } from "fp-ts/function";
import { runRulesEngine } from "../../src/pipes/rules-engine.pipe";
import { task, taskEither } from "fp-ts";
import { EntityType } from "../../src/utils/enums/entity-type.enum";
import { genCreateContribInput } from "../utils/gen-create-contrib-input.util";
import { ILexisNexisConfig } from "../../src/clients/lexis-nexis/lexis-nexis.client";
import { millisToYearStart, now } from "../../src/utils/time.utils";
import { getStripeApiKey } from "../../src/utils/config";
import { runRulesAndProcess } from "../../src/pipes/run-rules-and-process.pipe";
import { Stripe } from "stripe";
import { ApplicationError } from "../../src/utils/application-error";

dotenv.config();

const billableEventsTableName = process.env.BILLABLE_EVENTS_DDB_TABLE_NAME;
const donorsTable = process.env.DONORS_DDB_TABLE_NAME;
const txnsTable = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const rulesTable = process.env.RULES_DDB_TABLE_NAME;
const comsTable = process.env.COMMITTEES_DDB_TABLE_NAME;
const lnUsername = process.env.LN_USERNAME;
const lnPassword = process.env.LN_PASSWORD;

const instantIdConfig: ILexisNexisConfig = {
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
    const contrib = genCreateContribInput({
      committeeId: committee.id,
      amount: 750001,
      entityType: EntityType.Ind,
    });

    const res = await pipe(
      runRulesEngine(false)(billableEventsTableName)(donorsTable)(txnsTable)(
        rulesTable
      )(dynamoDB)(instantIdConfig)(committee)(contrib),
      taskEither.fold(
        (e) => {
          return task.of(e.data.remaining);
        },
        (s) => task.of("worked")
      )
    )();

    expect(res).to.equal(750000);
  });

  it("Allows a contribution within committee's limit", async () => {
    await putCommittee(comsTable)(dynamoDB)(committee);

    const contrib = genCreateContribInput({
      committeeId: committee.id,
      amount: 750000,
      entityType: EntityType.Ind,
    });

    const res = await pipe(
      runRulesEngine(false)(billableEventsTableName)(donorsTable)(txnsTable)(
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

  describe("Aggregate Duration", function () {
    it("Stops contribution exceeding aggregate duration of calendar year for corp donors", async () => {
      // Set Up

      const ps = new AWS.SSM();
      const stripeApiKey = await getStripeApiKey(ps)("qa");
      const stripe = new Stripe(stripeApiKey, {
        apiVersion: "2020-08-27",
      });
      const currentUser = "me";

      const paymentDateOfToday = now();
      const paymentDateOfLastYear =
        millisToYearStart(paymentDateOfToday) - 1000;

      const thisYearContrib = genCreateContribInput({
        committeeId: committee.id,
        amount: 500000,
        entityType: EntityType.Corp,
        paymentDate: paymentDateOfToday,
      });

      const lastYearContrib = {
        ...thisYearContrib,
        paymentDate: paymentDateOfLastYear,
        amount: 500001,
      };

      // Run

      const stagingRes = await pipe(
        runRulesAndProcess({
          allowInvalid: false,
          idVerifyEnabled: true,
        })(billableEventsTableName)(donorsTable)(txnsTable)(rulesTable)(
          dynamoDB
        )(stripe)(instantIdConfig)(currentUser)(committee)(thisYearContrib),
        taskEither.fold(
          (e) => task.of("fail"),
          (res) => task.of("success")
        )
      )();

      if (stagingRes === "fail")
        throw new ApplicationError("staging failed", {});

      const res = await pipe(
        runRulesEngine(false)(billableEventsTableName)(donorsTable)(txnsTable)(
          rulesTable
        )(dynamoDB)(instantIdConfig)(committee)(lastYearContrib),
        taskEither.fold(
          (e) => {
            return task.of(e.data.remaining);
          },
          (s) => task.of("worked")
        )
      )();

      expect(res).to.equal(500000);
    });
  });
});
