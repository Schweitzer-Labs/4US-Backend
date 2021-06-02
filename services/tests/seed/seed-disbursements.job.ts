import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { disbursementsData } from "./disbursements.data";
import * as dotenv from "dotenv";

dotenv.config();

const tableName = process.env.TRANSACTIONS_DDB_TABLE_NAME;

const run = async (dynamoDB: DynamoDB) => {
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
const dynamoDB = new DynamoDB();

run(dynamoDB).then(console.log).catch(console.log);
