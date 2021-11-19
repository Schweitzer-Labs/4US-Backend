import { expect } from "chai";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { ILexisNexisConfig } from "../../src/clients/lexis-nexis/lexis-nexis.client";
import { DynamoDB } from "aws-sdk";
import { Stripe } from "stripe";
import { genCommittee } from "../utils/gen-committee.util";
import { getStripeApiKey } from "../../src/utils/config";
import { putCommittee } from "../../src/utils/model/committee/put-committee.utils";
import { sleep } from "../../src/utils/sleep.utils";
import { syncActBlue } from "../../src/external-data/act-blue.external-data";
import {
  ActBlueCSVType,
  IActBluePaidContribution,
} from "../../src/clients/actblue/actblue.decoders";
import { nMonthsAgo, now } from "../../src/utils/time.utils";
import {
  actBlueCSVMetadataToTypedData,
  getActBlueCSVMetadata,
} from "../../src/clients/actblue/actblue.client";
import { isLeft } from "fp-ts/Either";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { IExternalContrib } from "../../src/model/external-data.type";
import { mLog } from "../../src/utils/m-log.utils";

dotenv.config();

const billableEventsTableName = process.env.BILLABLE_EVENTS_DDB_TABLE_NAME;
const donorsTable = process.env.DONORS_DDB_TABLE_NAME;
const txnsTable = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const rulesTable = process.env.RULES_DDB_TABLE_NAME;
const comsTable = process.env.COMMITTEES_DDB_TABLE_NAME;

const lnUsername = process.env.LN_USERNAME;
const lnPassword = process.env.LN_PASSWORD;
const actBlueSecret = process.env.ACTBLUE_CLIENT_SECRET;
const actBlueUuid = process.env.ACTBLUE_CLIENT_UUID;
const actBlueAccountId = process.env.ACTBLUE_ACCOUNT_ID;
const ps = new AWS.SSM();
let stripe: Stripe;

const lnConfig: ILexisNexisConfig = {
  username: lnUsername,
  password: lnPassword,
};

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const ddb = new DynamoDB();

const committee = genCommittee({
  district: "53",
  officeType: "senate",
  party: "democrat",
  race: "primary",
  ruleVersion: "nyboe-2020",
  scope: "state",
  state: "ny",
  tzDatabaseName: "America/New_York",
  actBlueAccountId,
  actBlueAPICredentials: {
    clientUUID: actBlueUuid,
    clientSecret: actBlueSecret,
  },
});

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const reportType = ActBlueCSVType.PaidContributions;

let res: IExternalContrib[];
describe("ActBlue to External Transaction Synchronization", function () {
  before(async () => {
    const stripeApiKey = await getStripeApiKey(ps)("qa");
    stripe = new Stripe(stripeApiKey, {
      apiVersion: "2020-08-27",
    });

    await putCommittee(comsTable)(ddb)(committee);
    const rn = now();
    const sixMAgo = nMonthsAgo(6)(rn);

    const eitherCsvMetadata = await getActBlueCSVMetadata(reportType)(
      committee.actBlueAPICredentials
    )(sixMAgo)(rn)();

    if (isLeft(eitherCsvMetadata))
      throw new Error("ActBlue csv request failed");

    await sleep(5000);

    if (isLeft(eitherCsvMetadata))
      throw new Error("ActBlue csv request failed");

    const eitherContribs = await pipe(
      actBlueCSVMetadataToTypedData(committee.actBlueAPICredentials)(
        eitherCsvMetadata.right.csvId
      ),
      taskEither.chain(mLog("csv data parsed")),
      taskEither.chain(
        syncActBlue(comsTable)(billableEventsTableName)(donorsTable)(txnsTable)(
          rulesTable
        )(ddb)(stripe)(lnConfig)
      )
    )();

    if (isLeft(eitherContribs))
      throw new Error("ActBlue committee sync failed");

    console.log("res is here");

    console.log(eitherContribs.right);

    res = eitherContribs.right;
  });
  it("Successfully imports ActBlue transactions", async () => {
    expect(res.length > 0).to.equal(true);
  });

  after(async () => {
    console.log("committee ID");
    console.log(committee.id);
    // await deleteCommittee(comsTable)(ddb)(committee);
  });
});
