import {
  ActBlueCSVType,
  ActBlueCSVUrlResponse,
  ActBlueGenCSVResponse,
  ActBluePaidContribution,
  IActBlueAPICredentials,
  IActBlueCSVMetadata,
  IActBlueCSVUrlResponse,
  IActBluePaidContribution,
} from "./actblue.decoders";
import { pipe } from "fp-ts/function";
import { taskEither as te } from "fp-ts";
import axios from "axios";
import { ApplicationError } from "../../utils/application-error";
import { TaskEither } from "fp-ts/TaskEither";
import { decodeRawData } from "../../utils/decode-raw-data.util";
import * as t from "io-ts";
import { mLog } from "../../utils/m-log.utils";
import { csvToTypedData } from "../../utils/parse-csv.utils";
import { toISODate } from "../../utils/time.utils";

const endpoint = "https://secure.actblue.com/api/v1/csvs";

const getActBlueCSVByUrlUnsafe = async (url: string): Promise<unknown> => {
  const { data } = await axios.get<unknown>(url);
  return data;
};

export const getActBlueCSVByUrl = (
  url: string
): TaskEither<ApplicationError, string> =>
  pipe(
    te.tryCatch(
      () => getActBlueCSVByUrlUnsafe(url),
      (err) => new ApplicationError("Get report by url failed", err)
    ),
    te.chain(decodeRawData("ActBlue CSV string")(t.string))
  );

const getActBlueCSVByIdUnsafe =
  (creds: IActBlueAPICredentials) =>
  async (reportId: string): Promise<unknown> => {
    const { data } = await axios.get<unknown>(`${endpoint}/${reportId}`, {
      auth: { username: creds.clientUUID, password: creds.clientSecret },
    });

    return data;
  };

const getActBlueCSVById =
  (creds: IActBlueAPICredentials) =>
  (reportId: string): TaskEither<ApplicationError, unknown> =>
    te.tryCatch(
      () => getActBlueCSVByIdUnsafe(creds)(reportId),
      (err) => new ApplicationError("Get report by url failed", err)
    );

export const actBlueCSVMetadataToTypedData =
  (reportId: string) =>
  (
    creds: IActBlueAPICredentials
  ): TaskEither<ApplicationError, IActBluePaidContribution[]> =>
    pipe(
      getActBlueCSVById(creds)(reportId),
      te.chain(mLog("Raw resp")),
      te.chain(
        decodeRawData("ActBlue CSV URL response")(ActBlueCSVUrlResponse)
      ),
      te.chain(mLog("Decoded CSV Status")),
      te.map((resp) => resp.download_url),
      te.chain(mLog("Got CSV url")),
      te.chain(getActBlueCSVByUrl),
      te.chain(
        csvToTypedData("ActBlue Contrib CSV")(t.array(ActBluePaidContribution))
      )
    );

const getActBlueCSVIdUnsafe =
  (csvType: ActBlueCSVType) =>
  (creds: IActBlueAPICredentials) =>
  (fromTime: number) =>
  async (toTime: number): Promise<unknown> => {
    const fromTimeISO = toISODate(fromTime);
    const toTimeISO = toISODate(toTime);
    const { data } = await axios.post<unknown>(
      endpoint,
      {
        csv_type: csvType,
        date_range_start: fromTimeISO,
        date_range_end: toTimeISO,
      },
      {
        auth: { username: creds.clientUUID, password: creds.clientSecret },
      }
    );

    return data;
  };

export const getActBlueCSVMetadata =
  (csvType: ActBlueCSVType) =>
  (creds: IActBlueAPICredentials) =>
  (fromTime: number) =>
  (toTime: number): TaskEither<ApplicationError, IActBlueCSVMetadata> =>
    pipe(
      te.tryCatch(
        () => getActBlueCSVIdUnsafe(csvType)(creds)(fromTime)(toTime),
        (err) => new ApplicationError("Call to generate ActBlue ID failed", err)
      ),
      te.chain(decodeRawData("ActBlue CSV ID response")(ActBlueGenCSVResponse)),
      te.map((res) => ({
        csvType,
        csvId: res.id,
      }))
    );
