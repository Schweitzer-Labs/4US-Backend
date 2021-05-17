import { data } from "./contributions.data";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { v4 as uuidv4 } from "uuid";
import {
  boolToDDBBool,
  numberToDDBNumber,
  stringToDDBString,
} from "./seed.utils";

const run = async (dynamoDB: DynamoDB, sequence: number) => {
  const list = data.slice(25 * sequence - 25, 25 * sequence);
  const tableName = "transactions-dev";
  const committeeId = "907b427a-f8a9-450b-9d3c-33d8ec4a4cc4";
  const now = new Date().getTime().toString();
  const items = list.map((txn) => ({
    PutRequest: {
      Item: {
        ...stringToDDBString("id", uuidv4()),
        ...boolToDDBBool("bankVerified", false),
        ...boolToDDBBool("ruleVerified", true),
        ...stringToDDBString("initiatedTimestamp", now),
        ...stringToDDBString("ruleVerifiedTimestamp", now),
        ...stringToDDBString("committeeId", committeeId),
        ...numberToDDBNumber("amount", txn.amount),
        ...stringToDDBString("firstName", txn.firstName),
        ...stringToDDBString("lastName", txn.lastName),
        ...stringToDDBString("employer", txn.employer),
        ...stringToDDBString("addressLine1", txn.addressLine1),
        ...stringToDDBString("addressLine2", txn.addressLine2),
        ...stringToDDBString("city", txn.city),
        ...stringToDDBString("state", txn.state),
        ...stringToDDBString("postalCode", txn.postalCode + ""),
        ...stringToDDBString("refCode", txn.refCode),
        ...stringToDDBString("paymentMethod", txn.paymentMethod),
        ...stringToDDBString("contributorType", txn.contributorType),
        ...stringToDDBString("direction", "in"),
      },
    },
  }));

  const res = await dynamoDB
    .batchWriteItem({
      RequestItems: {
        [tableName]: items,
      },
    })
    .promise();

  console.log(res);

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
