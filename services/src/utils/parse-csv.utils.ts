import csvToJson from "csvtojson";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "./application-error";
import { taskEither } from "fp-ts";
import { pipe } from "fp-ts/function";
import { decodeRawData } from "./decode-raw-data.util";
import * as t from "io-ts";

export const parseCSV = async (csv: string) => {
  return csvToJson().fromString(csv);
};

export const parseCSVAndDecode = (
  csv: string
): TaskEither<ApplicationError, any> =>
  taskEither.tryCatch(
    () => parseCSV(csv),
    (err) => new ApplicationError("Report is an invalid CSV", err)
  );

export const csvToTypedData =
  (prefix: string) =>
  <T>(type: t.Type<T>) =>
  (csv: string): TaskEither<ApplicationError, T> =>
    pipe(parseCSVAndDecode(csv), taskEither.chain(decodeRawData(prefix)(type)));
