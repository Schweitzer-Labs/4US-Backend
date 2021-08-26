import { SQSEvent } from "aws-lambda";

export default async (event: SQSEvent, context): Promise<any> => {
  console.log(JSON.stringify(event));
  console.log(event);
};
