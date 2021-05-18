const AWS = require("aws-sdk");
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
    params = {
      MessageAttributes: {
        committee: {
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
module.exports = async (event, context) => {
  for (const stream of event.Records) {
    const record = stream.dynamodb
      , data = AWS.DynamoDB.Converter.unmarshall(record.NewImage)
      , timestamp = new Date(record.ApproximateCreationDateTime * 1000)
    ;
    console.log("data", data);

    data.id = record.id;
    data.timezone = 'America/New_York';
    data.amount = (data.amount / 100).toFixed(2);
    data.timestamp = timestamp;
    data.state = data.state.toUpperCase();
    data.receipt = data.stripePaymentIntentId.slice(-8);

    data.occupation = data.occupation || '';
    data.employer = data.employer || '';
    data.refCode = data.refCode || 'N/A';

    await sendQueue(data);
  }
};
