import * as jsonexport from "jsonexport";
import { ITransaction } from "../queries/search-transactions.decoder";

export enum AggregateDuration {
  AGGREGATE_LIMIT = "aggregate_limit",
  CALENDAR_YEAR = "calendar_year",
}

export enum Field {
  DONOR_FULL_NAME = "donor_full_name",
  DONOR_ADDRESS = "donor_address",
  ENTITY_NAME = "entity_name",
  EMPLOYER_NAME = "employer_name",
  CPF_ID = "cpf_id",
  PRINCIPAL_OFFICER_FULL_NAME = "principal_officer_full_name",
}

export enum PaymentMethodType {
  ACH = "ach",
  Wire = "wire",
  Check = "check",
  Debit = "debit",
  Credit = "credit",
  Transfer = "transfer",
}

export enum ContributorType {
  CAN = "can",
  FAM = "fam",
  IND = "ind",
  SOLEP = "solep",
  PART = "part",
  CORP = "corp",
  COMM = "comm",
  UNION = "union",
  ASSOC = "assoc",
  LLC = "llc",
  PAC = "pac",
  PLC = "plc",
  OTH = "oth",
}

export const ContributorTypeDescription = new Map<string, string>([
  [ContributorType.CAN, "Candidate/Candidate Spouse"],
  [ContributorType.FAM, "Candidate Family Member"],
  [ContributorType.IND, "Individual"],
  [ContributorType.SOLEP, "Sole Proprietorship"],
  [ContributorType.PART, "Partnership, including LLPs"],
  [ContributorType.CORP, "Corporation"],
  [ContributorType.COMM, "Committee"],
  [ContributorType.UNION, "Union"],
  [ContributorType.ASSOC, "Association"],
  [ContributorType.LLC, "Professional/Limited Liability Company (PLLC/LLC)"],
  [ContributorType.PAC, "Political Action Committee (PAC)"],
  [ContributorType.PLC, "Political Committee"],
  [ContributorType.OTH, "Other"],
]);

export const NYSContributorTypeId = new Map<ContributorType, number>([
  [ContributorType.CAN, 1],
  [ContributorType.FAM, 2],
  [ContributorType.IND, 3],
  [ContributorType.SOLEP, 4],
  [ContributorType.PART, 5],
  [ContributorType.CORP, 6],
  [ContributorType.COMM, 7],
  [ContributorType.UNION, 9],
  [ContributorType.ASSOC, 10],
  [ContributorType.LLC, 11],
  [ContributorType.PAC, 12],
  [ContributorType.PLC, 13],
  [ContributorType.OTH, 14],
]);

export const NYSContributorTypeAggregateDuration = new Map<
  string,
  AggregateDuration
>([
  [ContributorType.FAM, AggregateDuration.AGGREGATE_LIMIT],
  [ContributorType.IND, AggregateDuration.AGGREGATE_LIMIT],
  [ContributorType.SOLEP, AggregateDuration.AGGREGATE_LIMIT],
  [ContributorType.PART, AggregateDuration.AGGREGATE_LIMIT],
  [ContributorType.CORP, AggregateDuration.CALENDAR_YEAR],
  [ContributorType.COMM, AggregateDuration.AGGREGATE_LIMIT],
  [ContributorType.UNION, AggregateDuration.AGGREGATE_LIMIT],
  [ContributorType.ASSOC, AggregateDuration.AGGREGATE_LIMIT],
  [ContributorType.LLC, AggregateDuration.CALENDAR_YEAR],
  [ContributorType.PAC, AggregateDuration.AGGREGATE_LIMIT],
  [ContributorType.PLC, AggregateDuration.AGGREGATE_LIMIT],
  [ContributorType.OTH, AggregateDuration.AGGREGATE_LIMIT],
]);

const scheduleAFields = [Field.DONOR_FULL_NAME, Field.DONOR_ADDRESS];

const scheduleBandCFields = [Field.ENTITY_NAME, Field.DONOR_ADDRESS];

export const NYSContributorTypeFields = new Map<string, Field[]>([
  [ContributorType.CAN, scheduleBandCFields],
  [ContributorType.FAM, scheduleAFields],
  [ContributorType.IND, scheduleAFields],
  [ContributorType.SOLEP, scheduleBandCFields],
  [ContributorType.PART, scheduleBandCFields],
  [ContributorType.CORP, scheduleBandCFields],
  [ContributorType.COMM, scheduleBandCFields],
  [ContributorType.UNION, scheduleBandCFields],
  [ContributorType.ASSOC, scheduleBandCFields],
  [ContributorType.LLC, scheduleBandCFields],
  [ContributorType.PAC, scheduleBandCFields],
  [ContributorType.PLC, scheduleBandCFields],
  [ContributorType.OTH, scheduleBandCFields],
]);

export const generateDisclosure = async (
  transactions: ITransaction[]
): Promise<string> => {
  const disclosures: DisclosureRecord[] = transactions.map(
    ({
      entityType: contributorType,
      id: transactionId,
      firstName,
      lastName,
      addressLine1,
      city,
      state,
      postalCode,
      paymentMethod,
      amount,
      companyName,
      entityName,
    }) => {
      return {
        // @Todo implement
        ["FILER_ID"]: 123,
        // @Todo implement
        ["FILING_PERIOD_ID"]: 123,
        // @Todo implement
        ["FILING_CAT_ID"]: 1,
        // @Todo implement
        ["ELECT_ID"]: 9,
        ["RESIG_TERM_TYPE_ID"]: "NULL",
        // @Todo implement
        ["R_FILING_DATE"]: "3-18-2021",
        ["FILING_SCHED_ID"]: 1,
        // getFilingScheduleIdByContributorType(contributorType),
        ["LOAN_LIB_NUMBER"]: "NULL",
        ["TRANS_NUMBER"]: transactionId,
        ["TRANS_MAPPING"]: "NULL",
        // @Todo implement
        ["SCHED_DATE"]: "3-18-2021",
        ["ORG_DATE"]: "NULL",
        ["CNTRBR_TYPE_ID"]: 3,
        // getContributorTypeIdByContributorType(contributorType),
        ["CNTRBN_TYPE_ID"]: "NULL",
        ["TRANSFER_TYPE_ID"]: "NULL",
        ["RECEIPT_TYPE_ID"]: "NULL",
        ["RECEIPT_CODE_ID"]: "NULL",
        ["PURPOSE_CODE_ID"]: "NULL",
        ["Is Expenditure Subcontracted?"]: "NULL",
        ["Is Expenditure a Partial Payment?"]: "NULL",
        ["Is this existing Liability?"]: "NULL",
        ["Is Liability a Partial Forgiven?"]: "NULL",
        ["FLNG_ENT_NAME"]: `${firstName} ${lastName}`,

        //   getEntityName(
        //   contributorType,
        //   firstName,
        //   lastName,
        //   companyName
        // ),
        ["FLNG_ENT_FIRST_NAME"]: firstName, // isPerson(contributorType) ? firstName : "NULL",
        ["FLNG_ENT_MIDDLE_NAME"]: "NULL",
        ["FLNG_ENT_LAST_NAME"]: lastName, // isPerson(contributorType) ? lastName : "NULL",
        ["FLNG_ENT_ADD1"]: addressLine1,
        ["FLNG_ENT_CITY"]: city,
        ["FLNG_ENT_STATE"]: state,
        ["FLNG_ENT_ZIP"]: postalCode,
        ["FLNG_ENT_COUNTRY"]: "US",
        ["PAYMENT_TYPE_ID"]: 1, //getPaymentTypeIdFromPaymentMethod(paymentMethod),
        ["PAY_NUMBER"]: "NULL",
        ["OWED_AMT"]: "NULL",
        ["ORG_AMT"]: centsToDollars(amount),
        ["TRANS_EXPLNTN"]: "NULL",
        ["LOAN_OTHER_ID"]: "NULL",
        ["R_ITEMIZED"]: "R_ITEMIZED",
        ["R_LIABILITY"]: "NULL",
        ["ELECTION_DATE"]: "NULL",
        ["ELECTION_TYPE"]: "NULL",
        ["ELECTION_YEAR"]: "NULL",
        ["TREAS_ID"]: "NULL",
        ["TREAS_OCCUPATION"]: "NULL",
        ["TREAS_EMPLOYER"]: "NULL",
        ["TREAS_ADD1"]: "NULL",
        ["TREAS_CITY"]: "NULL",
        ["TREAS_STATE"]: "NULL",
        ["TREAS_ZIP"]: "NULL",
        ["PART_FLNG_ENT_ID"]: "NULL",
        ["OFFICE_ID"]: "NULL",
        ["DISTRICT"]: "NULL",
        ["DIST_OFF_CAND_BAL_PROP"]: "NULL",
        ["IE_CNTRBR_OCC"]: "NULL",
        ["IE_CNTRBR_EMP"]: "NULL",
        ["IE_DESC"]: "NULL",
        ["R_IE_SUPPORTED"]: "NULL",
        ["R_IE_INCLUDED"]: "NULL",
        ["R_PARENT"]: "NULL",
      };
    }
  );

  return await jsonexport.default(disclosures);
};

const isPerson = (contributorType: ContributorType) =>
  [ContributorType.IND, ContributorType.FAM].includes(contributorType);

const getEntityName = (
  contributorType: ContributorType,
  firstName: string,
  lastName: string,
  companyName?: string
) => {
  if (isPerson(contributorType)) {
    return `${firstName} ${lastName}`;
  } else {
    return companyName;
  }
};

const getFilingScheduleIdByContributorType = (
  contributorType: ContributorType
): number => {
  switch (contributorType) {
    case ContributorType.IND:
    case ContributorType.PART:
      return 1;
    case ContributorType.CORP:
      return 2;
    default:
      return 3;
  }
};

const getContributorTypeIdByContributorType = (
  contributorType: ContributorType
) => {
  return NYSContributorTypeId.get(contributorType);
};

const getPaymentTypeIdFromPaymentMethod = (
  paymentMethod: PaymentMethodType
): number => {
  switch (paymentMethod) {
    case PaymentMethodType.Check:
      return 1;
    case PaymentMethodType.Credit:
      return 2;
    default:
      return 7;
  }
};

const centsToDollars = (amount: number): string => {
  return (amount / 100).toFixed(2);
};

interface DisclosureRecord {
  ["FILER_ID"]: number;
  ["FILING_PERIOD_ID"]: number;
  ["FILING_CAT_ID"]: number;
  ["ELECT_ID"]: number;
  ["RESIG_TERM_TYPE_ID"]: string;
  ["R_FILING_DATE"]: string;
  ["FILING_SCHED_ID"]: number;
  ["LOAN_LIB_NUMBER"]: string;
  ["TRANS_NUMBER"]: string;
  ["TRANS_MAPPING"]: string;
  ["SCHED_DATE"]: string;
  ["ORG_DATE"]: string;
  ["CNTRBR_TYPE_ID"]: number;
  ["CNTRBN_TYPE_ID"]: string;
  ["TRANSFER_TYPE_ID"]: string;
  ["RECEIPT_TYPE_ID"]: string;
  ["RECEIPT_CODE_ID"]: string;
  ["PURPOSE_CODE_ID"]: string;
  ["Is Expenditure Subcontracted?"]: string;
  ["Is Expenditure a Partial Payment?"]: string;
  ["Is this existing Liability?"]: string;
  ["Is Liability a Partial Forgiven?"]: string;
  ["FLNG_ENT_NAME"]: string;
  ["FLNG_ENT_FIRST_NAME"]: string;
  ["FLNG_ENT_MIDDLE_NAME"]: string;
  ["FLNG_ENT_LAST_NAME"]: string;
  ["FLNG_ENT_ADD1"]: string;
  ["FLNG_ENT_CITY"]: string;
  ["FLNG_ENT_STATE"]: string;
  ["FLNG_ENT_ZIP"]: string;
  ["FLNG_ENT_COUNTRY"]: string;
  ["PAYMENT_TYPE_ID"]: number;
  ["PAY_NUMBER"]: string;
  ["OWED_AMT"]: string;
  ["ORG_AMT"]: string;
  ["TRANS_EXPLNTN"]: string;
  ["LOAN_OTHER_ID"]: string;
  ["R_ITEMIZED"]: string;
  ["R_LIABILITY"]: string;
  ["ELECTION_DATE"]: string;
  ["ELECTION_TYPE"]: string;
  ["ELECTION_YEAR"]: string;
  ["TREAS_ID"]: string;
  ["TREAS_OCCUPATION"]: string;
  ["TREAS_EMPLOYER"]: string;
  ["TREAS_ADD1"]: string;
  ["TREAS_CITY"]: string;
  ["TREAS_STATE"]: string;
  ["TREAS_ZIP"]: string;
  ["PART_FLNG_ENT_ID"]: string;
  ["OFFICE_ID"]: string;
  ["DISTRICT"]: string;
  ["DIST_OFF_CAND_BAL_PROP"]: string;
  ["IE_CNTRBR_OCC"]: string;
  ["IE_CNTRBR_EMP"]: string;
  ["IE_DESC"]: string;
  ["R_IE_SUPPORTED"]: string;
  ["R_IE_INCLUDED"]: string;
  ["R_PARENT"]: string;
}
