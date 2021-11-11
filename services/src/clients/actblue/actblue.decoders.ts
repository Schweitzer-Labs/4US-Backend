import * as t from "io-ts";

export const ActBlueAPICredentials = t.type({
  clientUUID: t.string,
  clientSecret: t.string,
});

export type IActBlueAPICredentials = t.TypeOf<typeof ActBlueAPICredentials>;

export const ActBlueCSVUrlResponse = t.type({
  id: t.string,
  status: t.string,
  download_url: t.string,
});

export type IActBlueCSVUrlResponse = t.TypeOf<typeof ActBlueCSVUrlResponse>;

export const ActBlueGenCSVResponse = t.type({
  id: t.string,
});

export type IActBlueGenCSVResponse = t.TypeOf<typeof ActBlueGenCSVResponse>;

export const ActBluePaidContribution = t.type({
  ["Receipt ID"]: t.string,
  ["Date"]: t.string,
  ["Amount"]: t.string,
});

export type IActBluePaidContribution = t.TypeOf<typeof ActBluePaidContribution>;
