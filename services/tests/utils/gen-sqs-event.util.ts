import { SQSEvent } from "aws-lambda";

export const genSQSEventWithStr = (dataStr: string): SQSEvent => ({
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

export const genSQSEventWithBody = <a>(body: a): SQSEvent => ({
  Records: [
    {
      messageId: "abc",
      receiptHandle: "abc",
      body: JSON.stringify(body),
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
      awsRegion: "us-west-2",
    },
  ],
});
