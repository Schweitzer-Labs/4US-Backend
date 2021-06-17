import { APIGatewayEvent } from "aws-lambda";

export default async (event: APIGatewayEvent) => {
  const payload = JSON.parse(event.body);

  console.log(payload);

  switch (payload.type) {
    case "reporting.report_type.updated":
      console.log("reporting.report_type.updated event happened");
      break;
    default:
      console.log(`Unhandled event type ${payload.type}`);
  }
};
