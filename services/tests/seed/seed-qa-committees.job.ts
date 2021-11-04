import { DynamoDB } from "aws-sdk";
import * as AWS from "aws-sdk";
import { qaCommitteesData } from "./qa-committees.data";
import { putCommittee } from "../../src/utils/model/put-committee.utils";
import * as dotenv from "dotenv";
import { qaUsers } from "./qa-users.data";
import { ICommittee } from "../../src/model/committee.type";

AWS.config.update({ region: "us-west-2" });

dotenv.config();

const committeesTableName = process.env.COMMITTEES_DDB_TABLE_NAME;

const run =
  (committeesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (committees: ICommittee[]) => {
    console.log("running run");
    for (const committee of committees) {
      console.log("seeding committee " + committee.id);
      committee.members = [...committee.members, ...qaUsers];
      await putCommittee(committeesTableName)(dynamoDB)(committee);
      console.log(committee.id + " seeded");
    }
  };

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

export const seedCommittees = async () =>
  run(committeesTableName)(dynamoDB)(qaCommitteesData);

seedCommittees();
