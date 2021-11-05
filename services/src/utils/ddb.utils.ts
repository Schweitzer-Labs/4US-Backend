import * as t from "io-ts";
import { decodeRawData } from "./decode-raw-data.util";

export const ddbString = t.type({
  S: t.string,
});

export const ddbStringList = t.type({
  SS: t.array(t.string),
});

export const validateDDBResponse = decodeRawData;

export const toFilterExpression = (name: string, value?: any): string[] =>
  typeof value === "undefined" ? [] : [`${name} = :${name}`];

export const toExpressionAttributeValueString = (
  name: string,
  value?: string
): object =>
  typeof value === "undefined"
    ? {}
    : {
        [`:${name}`]: {
          S: value,
        },
      };

export const toExpressionAttributeValueBool = (
  name: string,
  value?: boolean
): object =>
  typeof value === "undefined"
    ? {}
    : {
        [`:${name}`]: {
          BOOL: value,
        },
      };
