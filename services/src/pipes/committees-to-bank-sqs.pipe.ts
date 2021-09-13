import { DynamoDB, SQS } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { getAll4USCommitteesAndDecode } from "../utils/model/get-all-4us-committees.utils";
import { taskEither } from "fp-ts";
import { ICommittee } from "../queries/get-committee-by-id.query";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { SendMessageRequest } from "aws-sdk/clients/sqs";
import { now } from "../utils/time.utils";

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
      sqs.sendMessage(toMsg(sqsUrl)(c));
      successCommittees.push(c);
    }
    return successCommittees;
  };
