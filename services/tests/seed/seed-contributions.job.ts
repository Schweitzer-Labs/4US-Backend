import { data } from "./contributions.data";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../src/model/transaction.type";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import { Source } from "../../src/utils/enums/source.enum";
import { Direction } from "../../src/utils/enums/direction.enum";
import { TransactionType } from "../../src/utils/enums/transaction-type.enum";
import * as dotenv from "dotenv";

dotenv.config();

const txnsTableName = process.env.TRANSACTIONS_DDB_TABLE_NAME;

const run = async (dynamoDB: DynamoDB, sequence: number) => {
  const list = data.slice(25 * sequenFce - 25, 25 * sequence);
  const committeeId = "pat-miller";
  const now = new Date().getTime();
  const items = list.map((txn) => {
    const contribution: ITransaction = {
      committeeId,
      id: genTxnId(),
      direction: Direction.In,
      amount: txn.amount * 100,
      paymentMethod: txn.paymentMethod,
      bankVerified: false,
      ruleVerified: true,
      initiatedTimestamp: now,
      ruleVerifiedTimestamp: now,
      refCode: txn.refCode,
      // Donor Props
      firstName: txn.firstName,
      lastName: txn.lastName,
      addressLine1: txn.addressLine1,
      addressLine2: txn.addressLine2,
      city: txn.city,
      state: txn.state,
      postalCode: txn.postalCode + "",
      employer: txn.employer,
      entityType: txn.contributorType,
      companyName: txn.companyName,
      attestsToBeingAnAdultCitizen: true,
      transactionType: TransactionType.Contribution,
      source: Source.FINICITY,
    };
    const marshalledContrib = DynamoDB.Converter.marshall(contribution);

    return {
      PutRequest: {
        Item: marshalledContrib,
      },
    };
  });

  return await dynamoDB
    .batchWriteItem({
      RequestItems: {
        [txnsTableName]: items,
      },
    })
    .promise();
};

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

run(dynamoDB, 1).then(console.log).catch(console.log);
run(dynamoDB, 2).then(console.log).catch(console.log);
run(dynamoDB, 3).then(console.log).catch(console.log);
run(dynamoDB, 4).then(console.log).catch(console.log);
