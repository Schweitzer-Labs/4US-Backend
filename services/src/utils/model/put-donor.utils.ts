import { DynamoDB } from "aws-sdk";
import { IDonor } from "../../model/donor.type";

export const putDonor =
  (donorsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (donor: IDonor): Promise<IDonor> => {
    const marshalledDonor = DynamoDB.Converter.marshall(donor);
    await dynamoDB
      .putItem({
        TableName: donorsTableName,
        Item: marshalledDonor,
      })
      .promise();
    return donor;
  };
