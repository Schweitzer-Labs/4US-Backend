import { DynamoDB } from "aws-sdk";
import { ICommittee } from "../../src/queries/get-committee-by-id.query";
import * as AWS from "aws-sdk";
import { prodCommitteesData } from "./prod/prod-committees.data";
import { putCommittee } from "../../src/utils/model/put-committee.utils";
import * as dotenv from "dotenv";

dotenv.config();

AWS.config.region = "us-east-1";
const committeesTableName = "prod-4us-backend-Committees-1HO9F0CX9Y3VL";

const run =
  (committeesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (committees: ICommittee[]) => {
    for (const committee of committees) {
      await putCommittee(committeesTableName)(dynamoDB)(committee);
    }
  };

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

export const seedCommittees = async () =>
  run(committeesTableName)(dynamoDB)(prodCommitteesData);

seedCommittees();
