import * as AWS from "aws-sdk";

import * as dotenv from "dotenv";
import { DynamoDB } from "aws-sdk";
import { isLeft } from "fp-ts/Either";
import { committeesToBankSQS } from "./pipes/committees-to-bank-sqs.pipe";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const ddb = new DynamoDB();
const sqs = new AWS.SQS({ apiVersion: "2012-11-05" });
const comsTable: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const bankSQSUrl: any = process.env.BANK_SQS_URL;

export default async () => {
  const res = await committeesToBankSQS(bankSQSUrl)(sqs)(comsTable)(ddb)();
  if (isLeft(res)) throw res.left;

  console.log("bank sync succeeded", res.right);
  return "success";
};
