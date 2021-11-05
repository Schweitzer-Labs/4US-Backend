import { expect } from "chai";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { putCommittee } from "../../src/utils/model/committee/put-committee.utils";
import { sleep } from "../../src/utils/sleep.utils";
import { DynamoDB } from "aws-sdk";
import { genCommittee } from "../utils/gen-committee.util";
import { deleteCommittee } from "../../src/utils/model/committee/delete-committee.utils";
import { externalTxnsToDdb } from "../../src/pipes/external-txns-to-ddb.pipe";
import {
  IExternalData,
  IExternalTxn,
} from "../../src/model/external-data.type";
import * as t from "io-ts";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import { now } from "../../src/utils/time.utils";
import * as faker from "faker";
import { enumToValues } from "../../src/utils/enums/poly.util";
import { State } from "../../src/utils/enums/state.enum";
import { searchTransactions } from "../../src/utils/model/transaction/search-transactions.query";
import { Order } from "../../src/utils/enums/order.enum";
import { TransactionType } from "../../src/utils/enums/transaction-type.enum";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";
import { ITransaction } from "../../src/model/transaction.type";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const ddb = new DynamoDB();

const comTable: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const txnTable: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;

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
});

const mockExternalTxn = (): IExternalTxn => ({
  recipientId: genTxnId(),
  source: "ActBlue",
  paymentDate: now(),
  amount: faker.datatype.number({
    min: 1000,
    max: 5000,
  }),
  firstName: faker.name.firstName(),
  lastName: faker.name.lastName(),
  addressLine1: faker.address.streetAddress(),
  city: faker.address.city(),
  state: faker.random.arrayElement(enumToValues(State)),
  postalCode: faker.address.zipCode(),
  country: "US",
});

const genList = (size: number) => Array.from(Array(size).keys());

const mockExternalData = (txnSize: number): IExternalData => ({
  transactions: genList(txnSize).map(mockExternalTxn),
});

const testSize = 10;
const testingData = mockExternalData(testSize);

describe("Syncs external data with a platform account", function () {
  before(async () => {
    await putCommittee(comTable)(ddb)(committee);

    await externalTxnsToDdb(txnTable)(ddb)(committee)(testingData)();
  });
  it("External transactions are saved to ", async () => {
    const txns = await pipe(
      searchTransactions(txnTable)(ddb)({
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

    console.log("transaction list");
    console.log(txns);

    expect(txns.length).to.equal(testSize);
  });
  after(async () => {
    await deleteCommittee(comTable)(ddb)(committee);
  });
});
