import { DynamoDB } from "aws-sdk";
import { ICommittee } from "../../src/queries/get-committee-by-id.query";
import * as AWS from "aws-sdk";
import { committeesData } from "./committees.data";
import { putCommittee } from "../../src/utils/model/put-committee.utils";

const run =
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (committees: ICommittee[]) => {
    for (const committee of committees) {
      await putCommittee(committeeTableName)(dynamoDB)(committee);
    }
  };

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

export const seedCommittees = async () =>
  run("committees-qa")(dynamoDB)(committeesData);
