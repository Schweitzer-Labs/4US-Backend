import { APIGatewayProxyEvent } from "aws-lambda";
import { Stripe } from "stripe";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { getStripeApiKey } from "./utils/config";
import { main } from "./either-tasks/main";
import { DynamoDB } from "aws-sdk";

dotenv.config();

const runenv: any = process.env.RUNENV;
const transactionsTableName: any = `transactions-${runenv}`;
const committeeTableName: any = `committees-${runenv}`;
let stripe: Stripe;
let stripeApiKey: string;
let dynamoDB: DynamoDB;

export default async (event: any) => {
  console.log(event);
  if (!stripeApiKey || !stripe || !dynamoDB) {
    // Set up dependencies
    stripeApiKey = await getStripeApiKey(runenv);
    stripe = new Stripe(stripeApiKey, {
      apiVersion: "2020-08-27",
    });

    AWS.config.apiVersions = {
      dynamodb: "2012-08-10",
    };
    dynamoDB = new DynamoDB();
  }

  return main(committeeTableName)(transactionsTableName)(stripe)(dynamoDB)(
    event
  )();
};
