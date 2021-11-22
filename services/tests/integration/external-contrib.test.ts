import { expect } from "chai";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { putCommittee } from "../../src/utils/model/committee/put-committee.utils";
import { DynamoDB } from "aws-sdk";
import { genCommittee } from "../utils/gen-committee.util";
import { deleteCommittee } from "../../src/utils/model/committee/delete-committee.utils";
import { syncExternalContributions } from "../../src/pipes/external-contribs/external-txns-to-ddb.pipe";
import { IExternalContrib } from "../../src/model/external-data.type";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import { now } from "../../src/utils/time.utils";
import * as faker from "faker";
import { State } from "../../src/utils/enums/state.enum";
import { searchTransactions } from "../../src/utils/model/transaction/search-transactions.query";
import { Order } from "../../src/utils/enums/order.enum";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";
import { ILexisNexisConfig } from "../../src/clients/lexis-nexis/lexis-nexis.client";
import { getStripeApiKey } from "../../src/utils/config";
import { Stripe } from "stripe";
import { EntityType } from "../../src/utils/enums/entity-type.enum";
import { PaymentMethod } from "../../src/utils/enums/payment-method.enum";
import { getCommitteeByActBlueAccountIdAndDecode } from "../../src/utils/model/committee/get-committee-by-actblue-id.utils";
import { sleep } from "../../src/utils/sleep.utils";
import { Source } from "../../src/utils/enums/source.enum";

dotenv.config();

const billableEventsTableName = process.env.BILLABLE_EVENTS_DDB_TABLE_NAME;
const donorsTable = process.env.DONORS_DDB_TABLE_NAME;
const txnsTable = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const rulesTable = process.env.RULES_DDB_TABLE_NAME;
const comsTable = process.env.COMMITTEES_DDB_TABLE_NAME;
const lnUsername = process.env.LN_USERNAME;
const lnPassword = process.env.LN_PASSWORD;
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

const actBlueAccountId = genTxnId();

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
});

const processorData = {
  entityName: "ActBlue Technical Services",
  addressLine1: faker.address.streetAddress(),
  city: faker.address.city(),
  state: faker.random.arrayElement(Object.values(State)),
  postalCode: faker.address.zipCode(),
  country: "US",
  paymentDate: now(),
};

const mockExternalContrib = (): IExternalContrib => ({
  id: genTxnId(),
  recipientId: actBlueAccountId,
  processorFeeData: {
    amount: 60,
    ...processorData,
  },
  source: Source.ActBlue,
  paymentDate: now(),
  amount: faker.datatype.number({
    min: 1000,
    max: 5000,
  }),
  // processorFeeData:
  firstName: faker.name.firstName(),
  lastName: faker.name.lastName(),
  addressLine1: faker.address.streetAddress(),
  city: faker.address.city(),
  state: faker.random.arrayElement(Object.values(State)),
  postalCode: faker.address.zipCode(),
  country: "US",
  entityType: EntityType.Ind,
  paymentMethod: PaymentMethod.Credit,
});

const genList = (size: number) => Array.from(Array(size).keys());

const mockExternalData = (txnSize: number): IExternalContrib[] =>
  genList(txnSize).map(mockExternalContrib);

const testSize = 3;
const testingData = mockExternalData(testSize);

describe("Syncs external contributions with a platform account", function () {
  before(async () => {
    const stripeApiKey = await getStripeApiKey(ps)("qa");
    stripe = new Stripe(stripeApiKey, {
      apiVersion: "2020-08-27",
    });

    await putCommittee(comsTable)(ddb)(committee);

    await sleep(2000);

    await syncExternalContributions({
      committeesTable: comsTable,
      billableEventsTable: billableEventsTableName,
      donorsTable: donorsTable,
      transactionsTable: txnsTable,
      rulesTable: rulesTable,
      dynamoDB: ddb,
      stripe: stripe,
      lexisNexisConfig: lnConfig,
      committeeValidator: (com) => (id) => com.actBlueAccountId === id,
      contributionMapper: (val) => val,
    })(committee.id)(testingData)();
  });
  it("External transactions are saved to database", async () => {
    const txns = await pipe(
      searchTransactions(txnsTable)(ddb)({
        committeeId: committee.id,
        order: Order.Asc,
        bankVerified: false,
        ruleVerified: true,
      }),
      taskEither.fold(
        () => task.of([]),
        (txns) => task.of(txns)
      )
    )();

    expect(txns.length).to.equal(testSize * 2);
  });
  after(async () => {
    console.log("committee Id");
    console.log(`http://localhost:3000/committee/${committee.id}`);
    // await deleteCommittee(comsTable)(ddb)(committee);
  });
});
