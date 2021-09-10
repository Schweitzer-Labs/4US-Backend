import * as t from "io-ts";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";
import { pipe } from "fp-ts/function";
import { taskEither, taskEither as te } from "fp-ts";
import { decodeError } from "../../utils/decode-error.util";
import csvToJson from "csvtojson";

export const ReportRunEvent = t.type({
  type: t.string,
  data: t.type({
    object: t.type({
      parameters: t.type({
        connected_account: t.string,
      }),
      report_type: t.string,
      result: t.type({
        url: t.string,
      }),
    }),
  }),
});

export const decodeReportRunEvent = (
  res: unknown
): TaskEither<ApplicationError, IReportRunEvent> => {
  return pipe(
    te.fromEither(ReportRunEvent.decode(res)),
    te.mapLeft(decodeError("ReportRunEvent"))
  );
};

export const reportEventToUrl = (event: IReportRunEvent): string =>
  event.data.object.result.url;

export const reportEventToStripeAccount = (event: IReportRunEvent): string =>
  event.data.object.parameter.connected_account;

export type IReportRunEvent = t.TypeOf<typeof ReportRunEvent>;

export const PayoutReportRow = t.type({
  automatic_payout_id: t.string,
  automatic_payout_effective_at_utc: t.string,
  balance_transaction_id: t.string,
  connected_account: t.string,
  charge_id: t.string,
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
