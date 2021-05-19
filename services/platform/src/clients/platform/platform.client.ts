import { Client } from "@elastic/elasticsearch";
import { left, TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";
import { PlatformTransaction } from "./platform.decoders";
import { pipe } from "fp-ts/function";

export const getUnverifiedTransactions =
  (client: Client) =>
  (
    committeeId: string
  ): TaskEither<ApplicationError, PlatformTransaction[]> => {
    console.log(committeeId);
    return pipe(
      tryCatch(
        () => queryTransactions(client)(committeeId),
        (e) =>
          new ApplicationError("Transaction query failed:", JSON.stringify(e))
      )
    );
  };

const queryTransactions = (client: Client) => async (committeeId: string) => {
  console.log(committeeId);
  const body = await client.search({
    size: 5000,
    body: {
      query: {
        bool: {
          must: [{ match: { committeeId } }],
        },
      },
    },
  });
  return [];
};
