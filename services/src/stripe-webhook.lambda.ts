import { APIGatewayEvent, APIGatewayProxyResult } from "aws-lambda";

export default async (
  event: APIGatewayEvent
): Promise<APIGatewayProxyResult> => {
  const payload = JSON.parse(event.body);

  console.log(payload);

  switch (payload.type) {
    case "reporting.report_type.updated":
      console.log("reporting.report_type.updated event happened");
      break;
    default:
      console.log(`Unhandled event type ${payload.type}`);
  }

  return {
    statusCode: 200,
    body: "",
  };
};
