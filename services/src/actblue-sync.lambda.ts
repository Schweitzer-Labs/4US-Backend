import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";

import * as dotenv from "dotenv";
import { actBlueToSqs } from "./pipes/actblue-to-sqs.pipe";
import { isLeft } from "fp-ts/Either";
import { ActBlueCSVType } from "./clients/actblue/actblue.decoders";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const ddb = new DynamoDB();
const sqs = new AWS.SQS({ apiVersion: "2012-11-05" });
const comsTable: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const bankSQSUrl: any = process.env.ACTBLUE_SQS_URL;

export default async () => {
  // const res = await actBlueToSqs(ActBlueCSVType.PaidContributions)(bankSQSUrl)(
  //   sqs
  // )(comsTable)(ddb)();
  // if (isLeft(res)) throw res.left;

  return "success";
};
