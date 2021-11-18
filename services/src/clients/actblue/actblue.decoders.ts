import * as t from "io-ts";
import { fromEnum } from "../../utils/from-enum.utils";
import { State } from "../../utils/enums/state.enum";

export enum ActBlueCSVType {
  PaidContributions = "paid_contributions",
  RefundedContributions = "refunded_contributions",
  ManagedFormContributions = "managed_form_contributions",
}

export interface IActBlueCSVMetadata {
  csvType: ActBlueCSVType;
  csvId: string;
}

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

export const ActBluePaidContributionReq = t.type({
  ["Receipt ID"]: t.string,
  ["Date"]: t.string,
  ["Amount"]: t.string,
  ["Fee"]: t.string,
  ["Donor First Name"]: t.string,
  ["Donor Last Name"]: t.string,
  ["Donor Addr1"]: t.string,
  ["Donor City"]: t.string,
  ["Donor State"]: fromEnum<State>("State", State),
  ["Donor ZIP"]: t.string,
  ["Donor Country"]: t.string,
  ["Payment ID"]: t.string,
  ["Payment Date"]: t.string,
  ["Disbursement ID"]: t.string,
  ["Disbursement Date"]: t.string,
  ["Recipient ID"]: t.string,
});

export const ActBluePaidContributionOpt = t.partial({
  ["Recurring Total Months"]: t.string,
  ["Recurrence Number"]: t.string,
  ["Recipient"]: t.string,
  ["Fundraising Page"]: t.string,
  ["Fundraising Partner"]: t.string,
  ["Reference Code 2"]: t.string,
  ["Reference Code"]: t.string,
  ["Donor Occupation"]: t.string,
  ["Donor Employer"]: t.string,
  ["Donor Email"]: t.string,
  ["Donor Addr2"]: t.string,
  ["Donor Phone"]: t.string,
  ["New Express Signup"]: t.string,
  ["Comments"]: t.string,
  ["Check Number"]: t.string,
  ["Check Date"]: t.string,
  ["Employer Addr1"]: t.string,
  ["Employer Addr2"]: t.string,
  ["Employer City"]: t.string,
  ["Employer State"]: fromEnum<State>("State", State),
  ["Employer ZIP"]: t.string,
  ["Employer Country"]: t.string,
  ["Donor ID"]: t.string,
  ["Fundraiser ID"]: t.string,
  ["Fundraiser Recipient ID"]: t.string,
  ["Fundraiser Contact Email"]: t.string,
  ["Fundraiser Contact First Name"]: t.string,
  ["Fundraiser Contact Last Name"]: t.string,
  ["Partner ID"]: t.string,
  ["Partner Contact Email"]: t.string,
  ["Partner Contact First Name"]: t.string,
  ["Partner Contact Last Name"]: t.string,
  ["Lineitem ID"]: t.string,
  ["AB Test Name"]: t.string,
  ["AB Variation"]: t.string,
  ["Recipient Committee"]: t.string,
  ["Recipient Gov ID"]: t.string,
  ["Recipient Election"]: t.string,
  ["Recovery ID"]: t.string,
  ["Recovery Date"]: t.string,
  ["Refund ID"]: t.string,
  ["Refund Date"]: t.string,
  ["Fee"]: t.string,
  ["Recur Weekly"]: t.string,
  ["ActBlue Express Lane"]: t.string,
  ["Card Type"]: t.string,
  ["Mobile"]: t.string,
  ["Recurring Upsell Shown"]: t.string,
  ["Recurring Upsell Succeeded"]: t.string,
  ["Double Down"]: t.string,
  ["Smart Recurring"]: t.string,
  ["Monthly Recurring Amount"]: t.string,
  ["Apple Pay"]: t.string,
  ["Card Replaced by Account Updater"]: t.string,
  ["ActBlue Express Donor"]: t.string,
  ["Custom Field 1 Label"]: t.string,
  ["Custom Field 1 Value"]: t.string,
  ["Donor US Passport Number"]: t.string,
  ["Text Message Opt In"]: t.string,
  ["Gift Identifier"]: t.string,
  ["Gift Declined"]: t.string,
  ["Shipping Addr1"]: t.string,
  ["Shipping City"]: t.string,
  ["Shipping State"]: t.string,
  ["Shipping Zip"]: t.string,
  ["Shipping Country"]: t.string,
  ["Weekly Recurring Amount"]: t.string,
  ["Smart Boost Amount"]: t.string,
  ["Smart Boost Shown"]: t.string,
});

export const ActBluePaidContribution = t.intersection([
  ActBluePaidContributionReq,
  ActBluePaidContributionOpt,
]);

export type IActBluePaidContribution = t.TypeOf<typeof ActBluePaidContribution>;

export const formatDate = (dateStr: string): number =>
  new Date(dateStr).getTime();
