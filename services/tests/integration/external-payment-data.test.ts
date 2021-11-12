import { expect } from "chai";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { putCommittee } from "../../src/utils/model/committee/put-committee.utils";
import { DynamoDB } from "aws-sdk";
import { genCommittee } from "../utils/gen-committee.util";
import { deleteCommittee } from "../../src/utils/model/committee/delete-committee.utils";
import { externalTxnsToDdb } from "../../src/pipes/external-txns-to-ddb.pipe";
import {
  IExternalData,
  IExternalTxn,
} from "../../src/model/external-data.type";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import { now } from "../../src/utils/time.utils";
import * as faker from "faker";
import { State } from "../../src/utils/enums/state.enum";
import { searchTransactions } from "../../src/utils/model/transaction/search-transactions.query";
import { Order } from "../../src/utils/enums/order.enum";
import { TransactionType } from "../../src/utils/enums/transaction-type.enum";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";
import { ILexisNexisConfig } from "../../src/clients/lexis-nexis/lexis-nexis.client";
import { getStripeApiKey } from "../../src/utils/config";
import { Stripe } from "stripe";

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
  finicityCustomerId: "5007489410",
  finicityAccountId: "5016000964",
  actBlueAccountId,
});

const mockExternalTxn = (amount?: number): IExternalTxn => ({
  id: genTxnId(),
  recipientId: actBlueAccountId,
  source: "ActBlue",
  paymentDate: now(),
  amount:
    amount ||
    faker.datatype.number({
      min: 1000,
      max: 5000,
    }),
  firstName: faker.name.firstName(),
  lastName: faker.name.lastName(),
  addressLine1: faker.address.streetAddress(),
  city: faker.address.city(),
  state: faker.random.arrayElement(Object.values(State)),
  postalCode: faker.address.zipCode(),
  country: "US",
});

const genList = (size: number) => Array.from(Array(size).keys());

const mockExternalData = (txnSize: number): IExternalData => ({
  transactions: genList(txnSize).map(mockExternalTxn),
});

const testSize = 3;
const testingData = mockExternalData(testSize);

describe("Syncs external data with a platform account", function () {
  before(async () => {
    const stripeApiKey = await getStripeApiKey(ps)("qa");
    stripe = new Stripe(stripeApiKey, {
      apiVersion: "2020-08-27",
    });

    await putCommittee(comsTable)(ddb)(committee);

    await externalTxnsToDdb(comsTable)(billableEventsTableName)(donorsTable)(
      txnsTable
    )(rulesTable)(ddb)(stripe)(lnConfig)(testingData)();
  });
  it("External transactions are saved to ", async () => {
    const txns = await pipe(
      searchTransactions(txnsTable)(ddb)({
        committeeId: committee.id,
        order: Order.Asc,
        transactionType: TransactionType.Contribution,
        bankVerified: false,
        ruleVerified: true,
      }),
      taskEither.fold(
        () => task.of([]),
        (txns) => task.of(txns)
      )
    )();

    expect(txns.length).to.equal(testSize);
  });
  after(async () => {
    await deleteCommittee(comsTable)(ddb)(committee);
  });
});
