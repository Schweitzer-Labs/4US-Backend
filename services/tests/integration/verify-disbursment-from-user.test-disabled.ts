import "reflect-metadata";
import { expect } from "chai";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { genCommittee } from "../utils/gen-committee.util";
import { putCommittee } from "../../src/utils/model/put-committee.utils";
import { sleep } from "../../src/utils/sleep.utils";
import { putTransaction } from "../../src/utils/model/put-transaction.utils";
import { genDisbursement } from "../utils/gen-disbursement.util";
import { verifyDisbursementFromUserAndPut } from "../../src/pipes/verify-disbursement-from-user.pipe";
import { VerifyDisbursementInput } from "../../src/input-object-types/verify-disbursement.input-type";
import { isLeft } from "fp-ts/Either";
import { PurposeCode } from "../../src/utils/enums/purpose-code.enum";
import { now } from "../../src/utils/time.utils";
import { ApplicationError } from "../../src/utils/application-error";

dotenv.config();

const committeesTableName = process.env.COMMITTEES_DDB_TABLE_NAME;
const txnsTableName = process.env.TRANSACTIONS_DDB_TABLE_NAME;

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

const committee = genCommittee({});

describe("Verify Disbursement Validator", function () {
  before(async () => {
    await putCommittee(committeesTableName)(dynamoDB)(committee);
    await sleep(1000);
  });

  it("Validates an existing disbursement", async () => {
    const disbr = genDisbursement({
      committeeId: committee.id,
      bankVerified: true,
      ruleVerified: false,
    });
    await putTransaction(txnsTableName)(dynamoDB)(disbr);
    const input = new VerifyDisbursementInput();
    input.addressLine1 = "123 Address";
    input.city = "new city";
    input.state = "pa";
    input.postalCode = "22132";
    input.purposeCode = PurposeCode.OFFICE;
    input.paymentDate = now();
    input.checkNumber = "123123123";
    input.isExistingLiability = true;
    input.isSubcontracted = true;
    input.isPartialPayment = true;

    const res = await verifyDisbursementFromUserAndPut(txnsTableName)(dynamoDB)(
      committee.id
    )(disbr.id)(input)();

    if (isLeft(res)) {
      throw new ApplicationError("Test failed", res);
    }

    expect(res.right.ruleVerified).to.equal(true);
  });
});
