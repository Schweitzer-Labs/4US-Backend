import { SQSEvent } from "aws-lambda";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

export default async (event: SQSEvent): Promise<any> => {
  return "ActBlue SQS function invoked";
};
