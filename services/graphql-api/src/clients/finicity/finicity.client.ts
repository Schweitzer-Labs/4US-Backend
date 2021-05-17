import fetch from "node-fetch";
import { ApplicationError } from "../../utils/application-error";
import {
  FinicityConfig,
  FinicityTransaction,
  GetFinicityTransactionsResponse,
} from "./finicity.decoders";
import { TaskEither, left, right, tryCatch } from "fp-ts/TaskEither";
import { task, taskEither } from "fp-ts";
import { pipe } from "fp-ts/function";
import { Errors, failures } from "io-ts";

const getToken = async (config: FinicityConfig) => {
  const finicityAuthUrl =
    "https://api.finicity.com/aggregation/v2/partners/authentication";

  const body = {
    partnerId: config.partnerId,
    partnerSecret: config.partnerSecret,
  };
  const tokenRes = await fetch(finicityAuthUrl, {
    method: "POST",
    headers: {
      "Finicity-App-Key": config.appKey,
      "Content-Type": "application/json",
      Accept: "application/json",
    },
    body: JSON.stringify(body),
  });
  const { token } = await tokenRes.json();
  return token;
};

const getTransactionsRes = (config: FinicityConfig) => async (
  customerId: string,
  accountId: string,
  epochFrom: number,
  epochTo: number
): Promise<any> => {
  const qs = `?fromDate=${epochFrom}&toDate=${epochTo}`;
  const apiUrl = `https://api.finicity.com/aggregation/v3/customers/${customerId}/accounts/${accountId}/transactions${qs}`;
  const token = await getToken(config);

  const res = await fetch(apiUrl, {
    method: "GET",
    headers: {
      "Finicity-App-Key": config.appKey,
      "Finicity-App-Token": token,
      "Content-Type": "application/json",
      Accept: "application/json",
    },
  });

  const data = await res.json();

  if (res.ok) {
    return data;
  } else {
    throw new Error(data.message);
  }
};

const validateTransactionsRes = (
  res: any
): TaskEither<ApplicationError, FinicityTransaction[]> => {
  return pipe(
    task.of(GetFinicityTransactionsResponse.decode(res)),
    taskEither.fold(
      (err: Errors) => {
        const props = err.map((err) => err.context.map(({ key }) => key));
        return left(new ApplicationError("Invalid finicity response", props));
      },
      (res) => right(res.transactions)
    )
  );
};

export const getTransactions = (config: FinicityConfig) => (
  customerId: string,
  accountId: string,
  epochFrom: number,
  epochTo: number
): TaskEither<ApplicationError, FinicityTransaction[]> =>
  pipe(
    tryCatch<ApplicationError, any>(
      () =>
        getTransactionsRes(config)(customerId, accountId, epochFrom, epochTo),
      (e: any) =>
        new ApplicationError("Get Finicity Transactions request failed", e)
    ),
    taskEither.chain(validateTransactionsRes)
  );
