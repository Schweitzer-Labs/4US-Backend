import { SQS } from "aws-sdk";
import { ITransaction } from "../queries/search-transactions.decoder";
import { pipe } from "fp-ts/function";
import { SendMessageRequest } from "aws-sdk/clients/sqs";
import { taskEither } from "fp-ts";
import { sendMessage } from "../utils/send-sqs.utils";
import { now } from "../utils/time.utils";

export const txnToSQS = (sqsUrl: string) => (sqs: SQS) => (txn: ITransaction) =>
  pipe(taskEither.of(toMsg(sqsUrl)(txn)), taskEither.chain(sendMessage(sqs)));

const toMsg =
  (sqsUrl: string) =>
  (txn: ITransaction): SendMessageRequest => ({
    MessageBody: JSON.stringify(txn),
    QueueUrl: sqsUrl,
    MessageGroupId: txn.committeeId,
    MessageDeduplicationId: `${now()}-${txn.id}`,
  });
