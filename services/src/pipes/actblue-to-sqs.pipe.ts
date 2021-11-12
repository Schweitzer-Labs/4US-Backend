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

interface IActBlueSQSMsgBody {
  committeeId: string;
  csvType: ActBlueCSVType;
  csvId: string;
}

const comToCreds = (com: ICommittee): IActBlueAPICredentials =>
  com.actBlueAPICredentials;

const credsToCsvMetadata =
  (csvType: ActBlueCSVType) =>
  (
    creds: IActBlueAPICredentials
  ): TaskEither<ApplicationError, IActBlueCSVMetadata> => {
    const rightNow = now();
    const sixMonthsAgo = nMonthsAgo(6)(rightNow);
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
      te.of(comToCreds(com)),
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
      getActBlueCommitteesAndDecode(comsTable)(ddb)
      // te.chain(actBlueCommitteesToSQS(csvType)(sqsUrl)(sqs))
    );
