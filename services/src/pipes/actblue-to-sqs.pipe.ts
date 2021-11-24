import { DynamoDB, SQS } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { ICommittee } from "../model/committee.type";
import {
  ActBlueCSVType,
  IActBlueAPICredentials,
  IActBlueCSVMetadata,
} from "../clients/actblue/actblue.decoders";
import { taskEither, taskEither as te } from "fp-ts";
import * as Array from "fp-ts/Array";
import { getActBlueCommitteesAndDecode } from "../utils/model/committee/get-actblue-committees.utils";
import { nMonthsAgo, now } from "../utils/time.utils";
import { getActBlueCSVMetadata } from "../clients/actblue/actblue.client";
import { SendMessageRequest } from "aws-sdk/clients/sqs";
import { sendMessage } from "../utils/send-sqs.utils";
import { mLog } from "../utils/m-log.utils";
import * as t from "io-ts";
import { fromEnum } from "../utils/from-enum.utils";
import { Source } from "../utils/enums/source.enum";

const reportMonthRange = 6;

export const ActBlueSQSMsgBody = t.type({
  committeeId: t.string,
  csvType: fromEnum<ActBlueCSVType>("ActBlueCSVType", ActBlueCSVType),
  csvId: t.string,
});

export type IActBlueSQSMsgBody = t.TypeOf<typeof ActBlueSQSMsgBody>;

const comToCreds = (com: ICommittee): IActBlueAPICredentials =>
  com.actBlueAPICredentials;

const credsToCsvMetadata =
  (csvType: ActBlueCSVType) =>
  (
    creds: IActBlueAPICredentials
  ): TaskEither<ApplicationError, IActBlueCSVMetadata> => {
    const rightNow = now();
    // Sometimes this isn't exactly 6 months ago and ActBlue gives us 422, so we add a day
    const sixMonthsAgo =
      nMonthsAgo(reportMonthRange)(rightNow) + 1000 * 60 * 60 * 24;
    return getActBlueCSVMetadata(csvType)(creds)(sixMonthsAgo)(rightNow);
  };

const csvMetadataToMsgBody =
  (com: ICommittee) =>
  (csvMetadata: IActBlueCSVMetadata): IActBlueSQSMsgBody => ({
    committeeId: com.id,
    csvType: csvMetadata.csvType,
    csvId: csvMetadata.csvId,
  });

const msgBodyToSendMsgReq =
  (com: ICommittee) =>
  (sqsUrl: string) =>
  (msgBody: IActBlueSQSMsgBody): SendMessageRequest => ({
    MessageBody: JSON.stringify(msgBody),
    QueueUrl: sqsUrl,
    MessageGroupId: com.id,
    MessageDeduplicationId: msgBody.csvId,
  });

const actBlueCommitteeToSQS =
  (csvType: ActBlueCSVType) =>
  (sqsUrl: string) =>
  (sqs: SQS) =>
  (com: ICommittee): TaskEither<ApplicationError, unknown> =>
    pipe(
      te.of(com),
      te.chain(mLog("Committee received: ")),
      te.map(comToCreds),
      te.chain(mLog("ActBlue API creds retrieved:")),
      te.chain(credsToCsvMetadata(csvType)),
      te.chain(mLog("CSV request completed. CSV Metadata:")),
      te.map(csvMetadataToMsgBody(com)),
      te.map(msgBodyToSendMsgReq(com)(sqsUrl)),
      te.chain(mLog("Sending Message")),
      te.chain(sendMessage(sqs)),
      te.chain(mLog("Message sent"))
    );

const actBlueCommitteesToSQS =
  (csvType: ActBlueCSVType) =>
  (sqsUrl: string) =>
  (sqs: SQS) =>
  (coms: ICommittee[]): TaskEither<ApplicationError, unknown[]> =>
    Array.traverse(taskEither.ApplicativeSeq)(
      actBlueCommitteeToSQS(csvType)(sqsUrl)(sqs)
    )(coms);

export const actBlueToSqs =
  (csvType: ActBlueCSVType) =>
  (sqsUrl: string) =>
  (sqs: SQS) =>
  (comsTable: string) =>
  (ddb: DynamoDB): TaskEither<ApplicationError, unknown[]> =>
    pipe(
      getActBlueCommitteesAndDecode(comsTable)(ddb),
      te.chain(mLog("ActBlue committees query executed")),
      te.chain(actBlueCommitteesToSQS(csvType)(sqsUrl)(sqs))
    );
