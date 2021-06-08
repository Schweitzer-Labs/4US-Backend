import { expect } from "chai";

import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import * as dotenv from "dotenv";
import { genCommittee } from "../utils/gen-committee.util";
import { putCommittee } from "../../src/utils/model/put-committee.utils";
import { genTransaction } from "../utils/gen-transaction.util";
import { Direction } from "../../src/utils/enums/direction.enum";
import { TransactionType } from "../../src/utils/enums/transaction-type.enum";
import { putTransaction } from "../../src/utils/model/put-transaction.utils";
import { finicityBankSync } from "../../src/pipes/finicity-bank-sync.pipe";
import { isLeft } from "fp-ts/Either";
import { ApplicationError } from "../../src/utils/application-error";
import { deleteCommittee } from "../../src/utils/model/delete-committee.utils";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

const config = {
  partnerId: process.env.FINICITY_PARTNER_ID,
  partnerSecret: process.env.FINICITY_PARTNER_SECRET,
  appKey: process.env.FINICITY_APP_KEY,
};

const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;

const committee = genCommittee({
  district: "53",
  officeType: "senate",
  party: "democrat",
  race: "primary",
  ruleVersion: "nyboe-2020",
  scope: "state",
  state: "ny",
  tzDatabaseName: "America/New_York",
  finicityCustomerId: "5003896371",
  finicityAccountId: "5015365202",
});

const tx1 = genTransaction({
  committeeId: committee.id,
  amount: 10000,
  ruleVerified: true,
  bankVerified: false,
  direction: Direction.Out,
  transactionType: TransactionType.Disbursement,
  paymentDate: 1612868400000,
});

describe("Synchs transactions with a platform account", function () {
  before(async () => {
    await putCommittee(committeesTableName)(dynamoDB)(committee);
    await putTransaction(txnsTableName)(dynamoDB)(tx1);
  });
  it("Adds transactions from finicity which does not match an existing transaction", async () => {
    const res = await finicityBankSync(config)(txnsTableName)(
      committeesTableName
    )(dynamoDB)();

    if (isLeft(res)) {
      throw new ApplicationError("sync failed", res.left);
    }

    const m = res.right;

    // prettier-ignore
    const unwind = Promise.all(
        m.map((f) => f())
        .map(async (f) => await f)
    );

    const either1 = await unwind;

    const either2 = await either1.map(async (eitherF) => {
      if (isLeft(eitherF)) {
        throw new ApplicationError("error", eitherF);
      }
      return eitherF.right.map(async (f) => await f());
    });

    const res2 = (await Promise.all(either2)).map(Promise.all);

    console.log(res2);

    expect(true).to.equal(false);
  });

  it("Matches a transaction and updates it with the finicity id", async () => {
    expect(true).to.equal(false);
  });

  after(async () => {
    await deleteCommittee(committeesTableName)(dynamoDB)(committee);
  });
});
