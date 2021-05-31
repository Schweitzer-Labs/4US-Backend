import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../queries/search-transactions.decoder";
import { IDonor } from "../../queries/search-donors.query";

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
