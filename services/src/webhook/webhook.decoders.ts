import * as t from "io-ts";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { pipe } from "fp-ts/function";
import { taskEither, taskEither as te } from "fp-ts";
import { decodeError } from "../utils/decode-error.util";
import csvToJson from "csvtojson";

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

export type IReportRunEvent = t.TypeOf<typeof ReportRunEvent>;

export const PayoutPaidEvent = t.type({
  type: t.string,
  account: t.string,
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

export const PayoutReportRow = t.type({
  automatic_payout_id: t.string,
  automatic_payout_effective_at_utc: t.string,
  balance_transaction_id: t.string,
  connected_account: t.string,
});

export const PayoutReportRows = t.array(PayoutReportRow);

export type IPayoutReportRows = t.TypeOf<typeof PayoutReportRows>;

export const decodePayoutReportRows = (
  res: unknown
): TaskEither<ApplicationError, IPayoutReportRows> => {
  return pipe(
    te.fromEither(PayoutReportRows.decode(res)),
    te.mapLeft(decodeError("PayoutReportRows"))
  );
};

export const parseCSV = async (csv: string) => {
  console.log(csv);
  const res = await csvToJson().fromString(csv);
  console.log(res);
  return res;
};

export const parseCSVAndDecode = (
  csv: string
): TaskEither<ApplicationError, any> =>
  taskEither.tryCatch(
    () => parseCSV(csv),
    (err) => new ApplicationError("Report is an invalid CSV", err)
  );
