import * as t from "io-ts";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";
import { pipe } from "fp-ts/function";
import { taskEither as te } from "fp-ts";
import { decodeError } from "../../utils/decode-error.util";

export const PayoutPaidEvent = t.type({
  account: t.string,
});

export type IPayoutPaidEvent = t.TypeOf<typeof PayoutPaidEvent>;

export const RunReportRes = t.type({
  status: t.string,
});

export type IRunReportRes = t.TypeOf<typeof RunReportRes>;

export const decodeRunReportRes = (
  res: unknown
): TaskEither<ApplicationError, IRunReportRes> => {
  return pipe(
    te.fromEither(RunReportRes.decode(res)),
    te.mapLeft(decodeError("RunReportRes"))
  );
};

export const decodePayoutPaidEvent = (
  res: unknown
): TaskEither<ApplicationError, IPayoutPaidEvent> => {
  return pipe(
    te.fromEither(PayoutPaidEvent.decode(res)),
    te.mapLeft(decodeError("PayoutPaidEvent"))
  );
};
