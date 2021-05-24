import { DynamoDB } from "aws-sdk";
import { ICommittee } from "../../src/queries/get-committee-by-id.query";
import * as AWS from "aws-sdk";
import { committeesData } from "./committees.data";

const run =
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (committees: ICommittee[]) => {
    for (const committee of committees) {
      const marshalledCommittee = DynamoDB.Converter.marshall(committee);
      const res = await dynamoDB
        .putItem({
          TableName: committeeTableName,
          Item: marshalledCommittee,
        })
        .promise();
    }
  };

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

run("committees-qa")(dynamoDB)(committeesData);
