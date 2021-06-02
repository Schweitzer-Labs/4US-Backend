import { DynamoDB } from "aws-sdk";
import { IDonor } from "../../queries/search-donors.decoder";

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
