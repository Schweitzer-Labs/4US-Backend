import * as AWS from "aws-sdk";
import { DynamoDBStreamEvent } from "aws-lambda";
import { SendMessageRequest } from "aws-sdk/clients/sqs";
import { Transactions } from "./queries/search-transactions.decoder";
import { isLeft } from "fp-ts/Either";
import { ApplicationError } from "./utils/application-error";
AWS.config.update({ region: process.env.REGION });
const sqs = new AWS.SQS({ apiVersion: "2012-11-05" });
/*
 * Helper Functions
 */
const getCommitteeDetails = async (committee) => {
  const emails = {
    "angel-cruz": ["seemant@schweitzerlabs.com", "evan@schweitzerlabs.com"],
    "john-safford": ["awsadmin@schweitzerlabs.com"],
  };
  return { emails: emails[committee], timezone: "America/New_York" };
}; // getCommitteeDetails()

const sendQueue = async (message) => {
  const { emails, timezone } = await getCommitteeDetails(message.committee),
    params: SendMessageRequest = {
      MessageAttributes: {
        committeeEmailAddress: {
          DataType: "String",
          StringValue: emails.join(","),
        },
        tzcommittee: {
          DataType: "String",
          StringValue: timezone,
        },
      },
      MessageBody: JSON.stringify(message),
      MessageDeduplicationId: message.id,
      MessageGroupId: message.committee,
      QueueUrl: process.env.SQSQUEUE,
    };
  const resp = await sqs.sendMessage(params).promise();
  console.log("sent SQS", resp);
}; // sendQueue()

/*
 * Main Function
 */
export default async (event: DynamoDBStreamEvent, context) => {
  for (const stream of event.Records) {
    const record = stream.dynamodb;
    const unmarshalledTxn = AWS.DynamoDB.Converter.unmarshall(record.NewImage);

    switch (stream.eventName) {
      case "INSERT":
        console.log("transaction inserted", JSON.stringify(event));
        break;
      // return await sendQueue(eitherTxn.right);
      case "MODIFY":
        console.log("transaction modified", JSON.stringify(event));
        return;
      default:
        return;
    }
  }
};
