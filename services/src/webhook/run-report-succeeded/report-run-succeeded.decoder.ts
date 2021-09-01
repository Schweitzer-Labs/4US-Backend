import * as t from "io-ts";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";
import { pipe } from "fp-ts/function";
import { taskEither, taskEither as te } from "fp-ts";
import { decodeError } from "../../utils/decode-error.util";
import csvToJson from "csvtojson";

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
