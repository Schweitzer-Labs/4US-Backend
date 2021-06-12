import { putTransaction } from "../../src/utils/put-transaction.utils";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import { ITransaction } from "../../src/queries/search-transactions.decoder";
import { Direction } from "../../src/utils/enums/direction.enum";
import { PaymentMethod } from "../../src/utils/enums/payment-method.enum";
import { now } from "../../src/utils/time.utils";
import { EntityType } from "../../src/utils/enums/entity-type.enum";
import { DynamoDB } from "aws-sdk";
import * as AWS from "aws-sdk";
import { TransactionType } from "../../src/utils/enums/transaction-type.enum";
import { Source } from "../../src/utils/enums/source.enum";

const run = async (dynamoDB: DynamoDB) => {
  const txn: ITransaction = {
    id: genTxnId(),
    source: Source.DONATE_FORM,
    committeeId: "907b427a-f8a9-450b-9d3c-33d8ec4a4cc4",
    direction: Direction.In,
    paymentMethod: PaymentMethod.Credit,
    bankVerified: false,
    ruleVerified: false,
    initiatedTimestamp: now(),
    firstName: "Chris",
    lastName: "Johnson",
    employer: "Google",
    addressLine1: "1429 Buckingham Road",
    city: "Woodside",
    state: "NY",
    postalCode: "11377",
    amount: 2000,
    cardNumberLastFourDigits: "1232",
    refCode: "home-page",
    entityType: EntityType.Ind,
    transactionType: TransactionType.Contribution,
    stripePaymentIntentId: "fjdsk234jsidjf",
  };
  return await putTransaction("transactions-qa")(dynamoDB)(txn);
};

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

run(dynamoDB).then(console.log).catch(console.log);
