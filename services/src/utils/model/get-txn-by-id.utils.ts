import { DynamoDB } from "aws-sdk";
import {
  ITransaction,
  Transaction,
} from "../../queries/search-transactions.decoder";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../application-error";
import { pipe } from "fp-ts/function";
import { StatusCodes } from "http-status-codes";
import { taskEither } from "fp-ts";
import { validateDDBResponse } from "../../repositories/ddb.utils";
import {
  Committee,
  committeeIdToDDBRes,
  ICommittee,
} from "../../queries/get-committee-by-id.query";

export const requestTxnById =
  (txnTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string) =>
  async (txnId: string): Promise<any> => {
    const res = await dynamoDB
      .getItem({
        TableName: txnTableName,
        Key: {
          id: {
            S: txnId,
          },
          committeeId: {
            S: committeeId,
          },
        },
      })
      .promise();
    return DynamoDB.Converter.unmarshall(res.Item);
  };

export const getTxnById =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committeeId: string) =>
  (txnId: string): TaskEither<ApplicationError, ITransaction> => {
    return pipe(
      tryCatch<ApplicationError, any>(
        () => requestTxnById(txnsTableName)(dynamoDB)(committeeId)(txnId),
        (e) => new ApplicationError("Get transaction request failed", e)
      ),
      taskEither.chain(validateDDBResponse(Transaction))
    );
  };
