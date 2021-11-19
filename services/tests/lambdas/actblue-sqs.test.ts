import { expect } from "chai";

import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import * as dotenv from "dotenv";
import { genCommittee } from "../utils/gen-committee.util";
import { deleteCommittee } from "../../src/utils/model/committee/delete-committee.utils";
import actBlueSqs from "../../src/actblue-sqs.lambda";
import { putCommittee } from "../../src/utils/model/committee/put-committee.utils";
import { SQSEvent } from "aws-lambda";
import { genSQSEventWithBody } from "../utils/gen-sqs-event.util";
import {
  ActBlueCSVType,
  IActBlueAPICredentials,
} from "../../src/clients/actblue/actblue.decoders";
import { IActBlueSQSMsgBody } from "../../src/pipes/actblue-to-sqs.pipe";
import { getActBlueCSVMetadata } from "../../src/clients/actblue/actblue.client";
import { nMonthsAgo, now } from "../../src/utils/time.utils";
import { isLeft } from "fp-ts/Either";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const dynamoDB = new DynamoDB();

const secret = process.env.ACTBLUE_CLIENT_SECRET;
const uuid = process.env.ACTBLUE_CLIENT_UUID;
const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;

const actBlueCreds = <IActBlueAPICredentials>{
  clientSecret: secret,
  clientUUID: uuid,
};

const committee = genCommittee({
  district: "53",
  officeType: "senate",
  party: "democrat",
  race: "primary",
  ruleVersion: "nyboe-2020",
  scope: "state",
  state: "ny",
  tzDatabaseName: "America/New_York",
  actBlueAPICredentials: actBlueCreds,
});

const reportType = ActBlueCSVType.PaidContributions;

let sqsEvent: SQSEvent;

describe("SQS Processor for committee's ActBlue Contributions", function () {
  before(async () => {
    await putCommittee(committeesTableName)(dynamoDB)(committee);
    const rn = now();
    const sixMAgo = nMonthsAgo(6)(rn);

    const eitherCsvMetadata = await getActBlueCSVMetadata(reportType)(
      actBlueCreds
    )(sixMAgo)(rn)();

    if (isLeft(eitherCsvMetadata))
      throw new Error("ActBlue csv request failed");

    sqsEvent = genSQSEventWithBody(<IActBlueSQSMsgBody>{
      committeeId: committee.id,
      csvType: ActBlueCSVType.PaidContributions,
      csvId: eitherCsvMetadata.right.csvId,
    });
  });
  it("ActBlue SQS runs successfully", async () => {
    const res = await actBlueSqs(sqsEvent);
    console.log(res);

    expect(res).to.equal("ActBlue SQS invocation complete");
  });

  after(async () => {
    await deleteCommittee(committeesTableName)(dynamoDB)(committee);
  });
});
