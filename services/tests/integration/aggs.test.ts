import { expect } from "chai";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { ApplicationError } from "../../src/utils/application-error";
import { genCommittee } from "../utils/gen-committee.util";
import { putCommittee } from "../../src/utils/model/committee/put-committee.utils";
import { genTransaction } from "../utils/gen-transaction.util";
import { putTransaction } from "../../src/utils/model/transaction/put-transaction.utils";
import { Direction } from "../../src/utils/enums/direction.enum";
import { getAggsByCommitteeId } from "../../src/utils/model/aggs/get-aggs.utils";
import { isLeft } from "fp-ts/Either";
import { TransactionType } from "../../src/utils/enums/transaction-type.enum";
import { sleep } from "../../src/utils/sleep.utils";
import { refreshAggs } from "../../src/pipes/refresh-aggs.pipe";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

const txnTable = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const comTable = process.env.COMMITTEES_DDB_TABLE_NAME;
const aggTable = process.env.AGGREGATES_DDB_TABLE_NAME;

const committee = genCommittee({});
let txn = genTransaction({
  committeeId: committee.id,
  bankVerified: true,
  ruleVerified: true,
  direction: Direction.In,
  transactionType: TransactionType.Contribution,
});
describe("Aggregates Cache", function () {
  before(async () => {
    console.log("Test committee", committee);
    await putCommittee(comTable)(dynamoDB)(committee);
    await putTransaction(txnTable)(dynamoDB)(txn);
    await sleep(1000);
    await refreshAggs(aggTable)(txnTable)(dynamoDB)(committee.id)();
  });
  it("Updates balance", async () => {
    const eitherAggs = await getAggsByCommitteeId(aggTable)(dynamoDB)(
      committee.id
    )();

    if (isLeft(eitherAggs)) throw new ApplicationError("Aggs Test failed", {});

    expect(eitherAggs.right.totalRaised).to.equal(txn.amount);
  });
});
