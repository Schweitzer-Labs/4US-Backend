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
import { deleteCommittee } from "../../src/utils/model/delete-committee.utils";
import bankSync from "../../src/bank-sync.lambda";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

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

describe("Syncs transactions with a platform account", function () {
  before(async () => {
    // await putCommittee(committeesTableName)(dynamoDB)(committee);
    // await putTransaction(txnsTableName)(dynamoDB)(tx1);
  });
  it("Sync runs successfully", async () => {
    const res = await bankSync();

    expect(res).to.equal("success");
  });

  // it("Adds transactions from finicity which does not match an existing transaction", async () => {
  //   const res = expect(true).to.equal(false);
  // });
  //
  // it("Matches a transaction and updates it with the finicity id", async () => {
  //   expect(true).to.equal(false);
  // });

  after(async () => {
    await deleteCommittee(committeesTableName)(dynamoDB)(committee);
  });
});
