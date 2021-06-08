import * as AWS from "aws-sdk";

import * as dotenv from "dotenv";
import { DynamoDB } from "aws-sdk";
import { finicityBankSync } from "./pipes/finicity-bank-sync.pipe";
import { FinicityConfig } from "./clients/finicity/finicity.decoders";
import {
  getFinicityAppKey,
  getFinicityPartnerId,
  getFinicityPartnerSecret,
} from "./utils/config";
import { isLeft } from "fp-ts/Either";
import { ApplicationError } from "./utils/application-error";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

// @ToDo Load from parameter store

const dynamoDB = new DynamoDB();
const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const runenv: any = process.env.RUNENV;

let partnerId: string;
let partnerSecret: string;
let appKey: string;
let finicityConfig: FinicityConfig;

export default async () => {
  if (!partnerId || !partnerSecret || !appKey) {
    partnerId = await getFinicityPartnerId(runenv);
    partnerSecret = await getFinicityPartnerSecret(runenv);
    appKey = await getFinicityAppKey(runenv);

    finicityConfig = {
      partnerId,
      partnerSecret,
      appKey,
    };
  }

  const res = await finicityBankSync(finicityConfig)(txnsTableName)(
    committeesTableName
  )(dynamoDB)();

  if (isLeft(res)) {
    throw new ApplicationError("sync failed", res.left);
  }

  const m = res.right;

  // prettier-ignore
  const unwind = Promise.all(
    m.map((f) => f())
      .map(async (f) => await f)
  );

  const either1 = await unwind;

  const either2 = await either1.map(async (eitherF) => {
    if (isLeft(eitherF)) {
      throw new ApplicationError("error", eitherF);
    }
    return eitherF.right.map(async (f) => await f());
  });

  const res2 = await Promise.all(either2);

  return "success";
};
