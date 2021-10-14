import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { requestTxnById } from "../../src/utils/model/get-txn-by-id.utils";
import * as dotenv from "dotenv";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const ddb = new DynamoDB({
  region: "us-west-2",
});

const run = async () => {
  const res = await requestTxnById("qa-4us-backend-Transactions-1JHQO9WAOZDJD")(
    ddb
  )("1627494510983-7jMvrf")("1627494510984-jKcnxs");

  console.log("res", res);
};

run();
