import * as AWS from "aws-sdk";

import * as dotenv from "dotenv";
import { DynamoDB } from "aws-sdk";
import { reconcileAll } from "./pipes/reconcile-contributions.pipe";
dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

// @ToDo Load from parameter store

const dynamoDB = new DynamoDB();
const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;

export default async () => {
  // const res = await reconcileAll(txnsTableName)(committeesTableName)(
  //   dynamoDB
  // )();
  // console.log(res);

  return "success";
};
