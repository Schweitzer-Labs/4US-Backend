import * as t from "io-ts";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";
import { pipe } from "fp-ts/function";
import { taskEither, taskEither as te } from "fp-ts";
import { decodeError } from "../../utils/decode-error.util";

export const ReportRunEvent = t.type({
  type: t.string,
  data: t.type({
    object: t.type({
      parameter: t.type({
        connected_account: t.string,
      }),
      report_type: t.string,
      result: t.type({
        url: t.string,
      }),
    }),
  }),
});

export const reportEventToUrl = (event: IReportRunEvent): string =>
  event.data.object.result.url;

export const reportEventToStripeAccount = (event: IReportRunEvent): string =>
  event.data.object.parameter.connected_account;

export type IReportRunEvent = t.TypeOf<typeof ReportRunEvent>;

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

export const decodeReportRunEvent = (
  res: unknown
): TaskEither<ApplicationError, IReportRunEvent> => {
  return pipe(
    te.fromEither(ReportRunEvent.decode(res)),
    te.mapLeft(decodeError("ReportRunEvent"))
  );
};
