import { DynamoDB } from "aws-sdk";
import { ICommittee } from "../../types/committee.type";

export const deleteCommittee =
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (committee: ICommittee) => {
    await dynamoDB
      .deleteItem({
        TableName: committeeTableName,
        Key: {
          id: {
            S: committee.id,
          },
        },
      })
      .promise();
    return committee;
  };
