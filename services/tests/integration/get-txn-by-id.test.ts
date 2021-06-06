import { expect } from "chai";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { genTransaction } from "../utils/gen-transaction.util";
import { putTransaction } from "../../src/utils/model/put-transaction.utils";
import { sleep } from "../../src/utils/sleep.utils";
import { getTxnById } from "../../src/utils/model/get-txn-by-id.utils";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { isLeft } from "fp-ts/Either";
import { genContributionRecord } from "../utils/gen-contribution.util";
import { genCommittee } from "../utils/gen-committee.util";
import { putCommittee } from "../../src/utils/model/put-committee.utils";

dotenv.config();
const txnTableName = process.env.TRANSACTIONS_DDB_TABLE_NAME;

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const committee = genCommittee({
  district: "53",
  officeType: "senate",
  party: "democrat",
  race: "primary",
  ruleVersion: "nyboe-2020",
  scope: "state",
  state: "ny",
  tzDatabaseName: "America/New_York",
});

const dynamoDB = new DynamoDB();

describe("Get transaction by id", function () {
  before(async () => {});
  it("Retrieve a transaction by an valid id", async () => {
    const txn = genContributionRecord(committee.id);

    await putTransaction(txnTableName)(dynamoDB)(txn);

    await sleep(1000);

    const res = await getTxnById(txnTableName)(dynamoDB)(committee.id)(
      txn.id
    )();

    if (isLeft(res)) {
      throw Error();
    }

    expect(res.right.amount).to.equal(txn.amount);
  });
});
