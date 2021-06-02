import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../queries/search-transactions.decoder";
import { ICommittee } from "../../queries/get-committee-by-id.query";

export const putCommittee =
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (committee: ICommittee) => {
    const marshalledCommittee = DynamoDB.Converter.marshall(committee);
    await dynamoDB
      .putItem({
        TableName: committeeTableName,
        Item: marshalledCommittee,
      })
      .promise();
    return committee;
  };
