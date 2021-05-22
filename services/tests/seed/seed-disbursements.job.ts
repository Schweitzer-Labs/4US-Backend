import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { disbursementsData } from "./disbursements.data";

const run = async (dynamoDB: DynamoDB) => {
  const tableName = "transactions-dev";
  const items = disbursementsData.map((txn) => {
    const marshalledContrib = DynamoDB.Converter.marshall(txn);
    return {
      PutRequest: {
        Item: marshalledContrib,
      },
    };
  });

  return await dynamoDB
    .batchWriteItem({
      RequestItems: {
        [tableName]: items,
      },
    })
    .promise();
};

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
AWS.config.update({ region: "us-east-1" });
const dynamoDB = new DynamoDB();

run(dynamoDB).then(console.log).catch(console.log);
