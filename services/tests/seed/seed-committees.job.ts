import { DynamoDB } from "aws-sdk";
import { ICommittee } from "../../src/queries/get-committee-by-id.query";
import * as AWS from "aws-sdk";
import { committeesData } from "./committees.data";
import { putCommittee } from "../../src/utils/model/put-committee.utils";
import * as dotenv from "dotenv";

dotenv.config();

const committeesTableName = process.env.COMMITTEES_DDB_TABLE_NAME;

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
  run(committeesTableName)(dynamoDB)(committeesData);

seedCommittees();
