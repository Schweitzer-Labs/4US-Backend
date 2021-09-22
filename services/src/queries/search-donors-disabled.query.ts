import { DynamoDB } from "aws-sdk";
import {
  genFlacspee,
  IFlacspeeInput,
} from "../utils/model/gen-donor-match.utils";

const getDonors =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (flacspeeInput: IFlacspeeInput) =>
  async (): Promise<any> => {
    const res = await dynamoDB
      .query({
        TableName: txnsTableName,
        KeyConditionExpression: "flacspeeMatch = :flacspeeMatch",
        ScanIndexForward: false,
        ExpressionAttributeValues: {
          ":flacspeeMatch": { S: genFlacspee(flacspeeInput) },
        },
      })
      .promise();
    return res.Items.map((item) => DynamoDB.Converter.unmarshall(item));
  };
interface Donor {
  firstName?: string;
  middleName?: string;
  lastName?: string;
  addressLine1: string;
  addressLine2?: string;
  city?: string;
  state?: string;
  postalCode?: string;
  employer?: string;
  occupation?: string;
  entityType?: string;
  companyName?: string;
  phoneNumber?: string;
  emailAddress?: string;
  attestsToBeingAnAdultCitizen?: string;
}
