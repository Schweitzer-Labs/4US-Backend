import { data } from "./contributions.data";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import {
  boolToDDBBool,
  numberToDDBNumber,
  stringToDDBString,
} from "./seed.utils";

const first25 = data.slice(0, 25);

const run = (env: string) => async (dynamoDB: DynamoDB) => {
  const items = first25.map((txn) => ({
    PutRequest: {
      Item: {
        amount: numberToDDBNumber("amount", txn.amount),
        timestampInitiated: stringToDDBString(
          "timestampInitiated",
          new Date().toString()
        ),
        paymentMethod: stringToDDBString("paymentMethod", txn.paymentMethod),
        ruleVerified: boolToDDBBool("ruleVerified", false),
        bankVerified: boolToDDBBool("bankVerified", false),
        firstName: stringToDDBString("firstName", txn.firstName),
        lastName: stringToDDBString("lastName", txn.lastName),
        employer: stringToDDBString("employer", txn.employer),
        addressLine1: stringToDDBString("addressLine1", txn.addressLine1),
        addressLine2: stringToDDBString("addressLine2", txn.addressLine2),
        city: stringToDDBString("city", txn.city),
        state: stringToDDBString("state", txn.state),
        postalCode: stringToDDBString("postalCode", txn.postalCode + ""),
        refCode: stringToDDBString("refCode", txn.refCode),
        entityType: stringToDDBString("entityType", txn.contributorType),
      },
    },
  }));

  const payload = {
    "committees-dev": items,
  };

  return "sadf";
};

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
AWS.config.update({ region: "us-east-1" });
const dynamoDB = new DynamoDB();

run("dev")(dynamoDB);
