import { getLNPassword, getLNUsername, getStripeApiKey } from "./utils/config";
import { Stripe } from "stripe";
import { IInstantIdConfig } from "./clients/lexis-nexis/lexis-nexis.client";
import * as dotenv from "dotenv";
import { platformContribute } from "./pipes/platform-contribute.pipe";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const dynamoDB = new DynamoDB();

const billableEventsTableName: any = process.env.BILLABLE_EVENTS_DDB_TABLE_NAME;
const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const donorsTableName: any = process.env.DONORS_DDB_TABLE_NAME;
const rulesTableName: any = process.env.RULES_DDB_TABLE_NAME;
const runenv: any = process.env.RUNENV;

let stripeApiKey: string;
let stripe: Stripe;
let lnUsername: string;
let lnPassword: string;

export default async (event: any) => {
  if (stripeApiKey || stripe || lnUsername || lnPassword) {
    stripeApiKey = await getStripeApiKey(runenv);
    lnUsername = await getLNUsername(runenv);
    lnPassword = await getLNPassword(runenv);
    stripe = new Stripe(stripeApiKey, {
      apiVersion: "2020-08-27",
    });
  }
  const instantIdConfig: IInstantIdConfig = {
    username: lnUsername,
    password: lnPassword,
  };

  return await platformContribute(billableEventsTableName)(donorsTableName)(
    committeesTableName
  )(txnsTableName)(rulesTableName)(dynamoDB)(stripe)(instantIdConfig)(event)();
};
