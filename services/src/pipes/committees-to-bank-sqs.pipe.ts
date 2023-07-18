import { DynamoDB, SQS } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { getAll4USCommitteesAndDecode } from "../utils/model/committee/get-all-4us-committees.utils";
import { taskEither } from "fp-ts";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { SendMessageRequest } from "aws-sdk/clients/sqs";
import { now } from "../utils/time.utils";
import { ICommittee } from "../model/committee.type";

export const committeesToBankSQS =
  (sqsUrl: string) =>
  (sqs: SQS) =>
  (committeesTable: string) =>
  (dynamoDB: DynamoDB) =>
    pipe(
      getAll4USCommitteesAndDecode(committeesTable)(dynamoDB),
      taskEither.chain(filterOutNonFinicityAccounts),
      taskEither.chain(publishToSQSAndDecode(sqsUrl)(sqs))
    );

const filterOutNonFinicityAccounts = (
  committees: ICommittee[]
): TaskEither<ApplicationError, ICommittee[]> =>
  taskEither.of(committees.filter((val) => !!val.finicityAccountId));

const toMsg =
  (sqsUrl: string) =>
  (c: ICommittee): SendMessageRequest => ({
    MessageBody: c.id,
    QueueUrl: sqsUrl,
    MessageGroupId: c.id,
    MessageDeduplicationId: `${now()}-${c.id}`,
  });

const publishToSQSAndDecode =
  (sqsUrl: string) =>
  (sqs: SQS) =>
  (committees: ICommittee[]): TaskEither<ApplicationError, ICommittee[]> =>
    taskEither.tryCatch(
      () => publishToSQS(sqsUrl)(sqs)(committees),
      (err) => new ApplicationError("Publish failed", err)
    );

const publishToSQS =
  (sqsUrl: string) =>
  (sqs: SQS) =>
  async (committees: ICommittee[]): Promise<ICommittee[]> => {
    const successCommittees = [];
    for (const c of committees) {
      console.log("Pushing committee to queue", c.id);
      const res = await sqs.sendMessage(toMsg(sqsUrl)(c)).promise();
      console.log("SQS Res", JSON.stringify(res));
      successCommittees.push(c);
    }
    return successCommittees;
  };
