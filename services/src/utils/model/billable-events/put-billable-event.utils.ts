import { DynamoDB } from "aws-sdk";

export enum BillableEventName {
  LexisNexisInstantIdLookUp = "lexis-nexis-instant-id-look-up",
  LexisNexisBusinessIdLookUp = "lexis-nexis-business-id-look-up",
  LexisNexisCitizenshipLookUp = "lexis-nexis-citizenship-id-look-up",
}

// Instant ID look up  : $0.75
// Instant ID business : $2.00
// Citizenship         : $0.50

interface IBillableEvent {
  id: string;
  committeeId: string;
  eventName: BillableEventName;
  cost: number;
  request: any;
  response: any;
}

export const putBillableEvent =
  (billableEventTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (billableEvent: IBillableEvent): Promise<IBillableEvent> => {
    console.log("Put billable event running");
    const marshalledBillableEvent = DynamoDB.Converter.marshall(billableEvent);
    await dynamoDB
      .putItem({
        TableName: billableEventTableName,
        Item: marshalledBillableEvent,
      })
      .promise();
    return;
  };
