import { DynamoDB } from "aws-sdk";
import { ITransaction, Transaction } from "../../model/transaction.type";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../application-error";
import { pipe } from "fp-ts/function";

import { taskEither } from "fp-ts";
import { validateDDBResponse } from "../ddb.utils";
import { isEmpty } from "./get-res-is-empty.utils";

const logPrefix = "Get Transaction by ID";

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
        ConsistentRead: true,
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
        (e) => {
          return new ApplicationError("Get transaction request failed", e);
        }
      ),
      taskEither.chain(isEmpty(logPrefix)),
      taskEither.chain(validateDDBResponse(logPrefix)(Transaction))
    );
  };
