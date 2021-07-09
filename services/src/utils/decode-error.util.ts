import * as t from "io-ts";
import { ApplicationError } from "./application-error";

export const decodeError =
  (prefix: string) =>
  (e: t.Errors): ApplicationError => {
    const missingKeys = e.map((e) =>
      e.context
        .map(({ key }) => key)
        .filter((val) => val !== "0")
        .join(" ")
    );
    return new ApplicationError(
      `${prefix}: Decode failed. Missing properties: - ${missingKeys}`,
      {}
    );
  };
