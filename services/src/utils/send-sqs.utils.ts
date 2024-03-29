import { SQS } from "aws-sdk";
import { SendMessageRequest } from "aws-sdk/clients/sqs";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "./application-error";
import { taskEither } from "fp-ts";

export const sendMessage =
  (sqs: SQS) =>
  (message: SendMessageRequest): TaskEither<ApplicationError, unknown> => {
    console.log(
      "message send attempted with payload: ",
      JSON.stringify(message)
    );
    return taskEither.tryCatch(
      () => sqs.sendMessage(message).promise(),
      (e) => new ApplicationError("SQS send failed", e)
    );
  };
