import { APIGatewayProxyEvent } from "aws-lambda";
import { Stripe } from "stripe";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { getStripeApiKey } from "./utils/config";
import { main } from "./either-tasks/main";
import { DynamoDB } from "aws-sdk";

dotenv.config();

const runenv: any = process.env.RUNENV;
let stripe: Stripe;
let stripeApiKey: string;
let dynamoDB: DynamoDB;

export default async (event: APIGatewayProxyEvent) => {
  if (!stripeApiKey || !stripe || !dynamoDB) {
    // Set up dependencies
    stripeApiKey = await getStripeApiKey(runenv);
    stripe = new Stripe(stripeApiKey, {
      apiVersion: "2020-08-27",
    });

    AWS.config.apiVersions = {
      dynamodb: "2012-08-10",
    };
    AWS.config.update({ region: "us-east-1" });
    dynamoDB = new DynamoDB();
  }

  return main(runenv)(stripe)(dynamoDB)(event)();
};
