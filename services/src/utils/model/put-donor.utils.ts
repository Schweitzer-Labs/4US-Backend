import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../queries/search-transactions.decoder";
import { IDonor } from "../../queries/get-donor.query";

export const putDonor =
  (donorTableName: string) => (dynamoDB: DynamoDB) => async (donor: IDonor) => {
    const marshalledDonor = DynamoDB.Converter.marshall(donor);
    return await dynamoDB
      .putItem({
        TableName: donorTableName,
        Item: marshalledDonor,
      })
      .promise();
  };
