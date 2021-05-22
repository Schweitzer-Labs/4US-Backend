import { data } from "./contributions.data";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { v4 as uuidv4 } from "uuid";
import {ITransaction} from "../../src/queries/search-transactions.decoder";

const run = async (dynamoDB: DynamoDB, sequence: number) => {
  const list = data.slice(25 * sequence - 25, 25 * sequence);
  const tableName = "transactions-dev";
  const committeeId = "907b427a-f8a9-450b-9d3c-33d8ec4a4cc4";
  const now = new Date().getTime().toString();
  const items = list.map((txn) => {
    const contribution: ITransaction = {
      committeeId,
      id: uuidv4(),
      direction: 'in',
      amount: txn.amount,
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
      transactionType: "contribution",
    }
    const marshalledContrib = DynamoDB.Converter.marshall(contribution);
    console.log(marshalledContrib)
    return {
      PutRequest: {
        Item: marshalledContrib
      },
    }
  });

  const res = await dynamoDB
    .batchWriteItem({
      RequestItems: {
        [tableName]: items,
      },
    })
    .promise();

  return res;
};

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
AWS.config.update({ region: "us-east-1" });
const dynamoDB = new DynamoDB();

run(dynamoDB, 1).then(console.log).catch(console.log);
run(dynamoDB, 2).then(console.log).catch(console.log);
run(dynamoDB, 3).then(console.log).catch(console.log);
run(dynamoDB, 4).then(console.log).catch(console.log);
