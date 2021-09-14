import { SQSEvent } from "aws-lambda";

export const genSQSEvent = (dataStr: string): SQSEvent => ({
  Records: [
    {
      messageId: "abc",
      receiptHandle: "abc",
      body: dataStr,
      attributes: {
        ApproximateReceiveCount: "abc",
        SentTimestamp: "abc",
        SenderId: "abc",
        ApproximateFirstReceiveTimestamp: "abc",
      },
      messageAttributes: {},
      md5OfBody: "abc",
      eventSource: "abc",
      eventSourceARN: "abc",
      awsRegion: "us-west-1",
    },
  ],
});
