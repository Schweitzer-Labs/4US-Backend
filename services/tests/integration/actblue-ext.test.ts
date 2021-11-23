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
import { ActBlueCSVType } from "../../src/clients/actblue/actblue.decoders";
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
import { deleteCommittee } from "../../src/utils/model/committee/delete-committee.utils";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";

dotenv.config();

const billableEventsTable: any = process.env.BILLABLE_EVENTS_DDB_TABLE_NAME;
const transactionsTable: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const committeesTable: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const donorsTable: any = process.env.DONORS_DDB_TABLE_NAME;
const rulesTable: any = process.env.RULES_DDB_TABLE_NAME;
const runenv: any = process.env.RUNENV;

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
  id: `actblue-${genTxnId()}`,
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

    await putCommittee(committeesTable)(ddb)(committee);
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
      actBlueCSVMetadataToTypedData(eitherCsvMetadata.right.csvId)(
        committee.actBlueAPICredentials
      ),
      taskEither.chain(mLog("csv data parsed")),
      taskEither.chain(
        syncActBlue({
          transactionsTable,
          billableEventsTable,
          rulesTable,
          donorsTable,
          committeesTable,
          dynamoDB: ddb,
          lexisNexisConfig: lnConfig,
          stripe,
        })(committee.id)
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
    console.log(committee.id);
    await deleteCommittee(committeesTable)(ddb)(committee);
  });
});
