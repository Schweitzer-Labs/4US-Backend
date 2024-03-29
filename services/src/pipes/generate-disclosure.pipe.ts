import * as jsonexport from "jsonexport";
import { IOwner, ITransaction } from "../model/transaction.type";
import { EntityType } from "../utils/enums/entity-type.enum";
import { PurposeCode } from "../utils/enums/purpose-code.enum";
import { TransactionType } from "../utils/enums/transaction-type.enum";
import { InKindType, PaymentMethod } from "../utils/enums/payment-method.enum";
import { AggregateDuration } from "../model/rule.type";
import { now } from "../utils/time.utils";
import { ApplicationError } from "../utils/application-error";
import { prepareOwners } from "../utils/owner-math.utils";
import { TaskEither } from "fp-ts/TaskEither";
import { taskEither } from "fp-ts";
import { ICommittee } from "../model/committee.type";

export const generateDisclosureOrError =
  (committee: ICommittee) =>
  (includeHeaders: boolean) =>
  (transactions: ITransaction[]): TaskEither<ApplicationError, string> =>
    taskEither.tryCatch(
      () => generateDisclosure(committee)(transactions)(includeHeaders),
      (err) => new ApplicationError("Disclosure report generation failed", err)
    );

// Timezone of offset for eastern time
const offset = 60 * 60 * 3;

export const generateDisclosure =
  (committee: ICommittee) =>
  (transactions: ITransaction[]) =>
  async (includeHeaders: boolean): Promise<string> => {
    const disclosures: DisclosureRecord[] = transactions
      .filter((txn) => {
        // @ToDo convert hardcode into data
        return (
          txn.paymentDate - offset >=
            new Date("May 19, 2021").getTime() - offset &&
          txn.paymentDate - offset <=
            new Date("December 30, 2021").getTime() - offset
        );
      })
      .reduce((acc, txn) => {
        const entry = toRow(committee)(txn);
        const attributedContribs = toAttributedContribs(committee)(txn);
        return [entry, ...attributedContribs, ...acc];
      }, []);

    return await jsonexport.default(disclosures, { includeHeaders });
  };

const boolToYesNo = (val: any) => (val ? "Y" : "N");
const shouldBeAttributed = ({ transactionType, entityType }: ITransaction) =>
  transactionType === TransactionType.Contribution &&
  (entityType === EntityType.Llc || entityType === EntityType.Plc);

const toAttributedTxnId = (txn: ITransaction) => (index: number) =>
  `${txn.id}-attr-${index}`;

export const toAttributedContribs =
  (committee: ICommittee) =>
  (txn: ITransaction): any[] => {
    if (!shouldBeAttributed(txn)) return [];
    const preparedOwners = prepareOwners(txn);

    const ownerRows = preparedOwners.reduce((acc, owner) => {
      const row = toAttributedRow(committee)(txn)(owner)(acc.length);

      return [row, ...acc];
    }, []);

    return ownerRows;
  };

const toAttributedRow =
  (committee: ICommittee) =>
  (txn: ITransaction) =>
  (owner: IOwner) =>
  (index: number) => {
    const {
      entityType: entityTypeStr,
      id: transactionId,
      paymentMethod: paymentMethodStr,
    } = txn;
    const paymentMethod: any = paymentMethodStr;
    // const filingPeriod = getFilingPeriod(committee);
    return {
      // @Todo implement
      ["FILER_ID"]: committee.efsFilerId,
      // @Todo implement
      ["FILING_PERIOD_ID"]: filingDates[2].id,
      ["FILING_CAT_ID"]: 1,
      ["ELECT_ID"]: committee.efsElectionId,
      ["RESIG_TERM_TYPE_ID"]: "NULL",
      // @Todo implement
      ["R_FILING_DATE"]: millisToDateStr(filingDates[2].filingDate),
      ["FILING_SCHED_ID"]: 15,
      ["LOAN_LIB_NUMBER"]: "NULL",
      ["TRANS_NUMBER"]: toAttributedTxnId(txn)(index),
      ["TRANS_MAPPING"]: transactionId,
      // @Todo implement
      ["SCHED_DATE"]: millisToDateStr(txn.paymentDate),
      ["ORG_DATE"]: "NULL",
      ["CNTRBR_TYPE_ID"]: "NULL",
      // @ToDo add in-kind field
      ["CNTRBN_TYPE_ID"]: "NULL",
      ["TRANSFER_TYPE_ID"]: "NULL",
      ["RECEIPT_TYPE_ID"]: "NULL",
      ["RECEIPT_CODE_ID"]: "NULL",
      ["PURPOSE_CODE_ID"]: "NULL",
      ["Is Expenditure Subcontracted?"]: "NULL",
      ["Is Expenditure a Partial Payment?"]: "NULL",
      ["Is this existing Liability?"]: "NULL",
      ["Is Liability a Partial Forgiven?"]: "NULL",
      ["FLNG_ENT_NAME"]: `${owner.firstName} ${owner.lastName}`,
      ["FLNG_ENT_FIRST_NAME"]: owner.firstName,
      ["FLNG_ENT_MIDDLE_NAME"]: "NULL",
      ["FLNG_ENT_LAST_NAME"]: owner.lastName,
      ["FLNG_ENT_ADD1"]: owner.addressLine1,
      ["FLNG_ENT_CITY"]: owner.city,
      ["FLNG_ENT_STATE"]: owner.state,
      ["FLNG_ENT_ZIP"]: owner.postalCode,
      ["FLNG_ENT_COUNTRY"]: "United States",
      ["PAYMENT_TYPE_ID"]: NYSPaymentTypeId.get(paymentMethod),
      ["PAY_NUMBER"]: "NULL",
      ["OWED_AMT"]: "NULL",
      ["ORG_AMT"]: centsToDollars(owner.attributedAmount),
      ["TRANS_EXPLNTN"]: txnToMemo(txn),
      ["LOAN_OTHER_ID"]: "NULL",
      ["R_ITEMIZED"]: "Y",
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
  };

const toRow = (committee: ICommittee) => (txn: ITransaction) => {
  const {
    entityType: entityTypeStr,
    id: transactionId,
    firstName,
    lastName,
    addressLine1,
    city,
    state,
    postalCode,
    paymentMethod: paymentMethodStr,
    amount,
    entityName,
    transactionType,
    purposeCode: purposeCodeString,
    isSubcontracted,
    isPartialPayment,
    isExistingLiability,
  } = txn;
  const entityType: any = entityTypeStr;
  const paymentMethod: any = paymentMethodStr;
  const purposeCode: any = purposeCodeString;
  // const filingPeriod = getFilingPeriod(committee);
  const inKindType: any = txn.inKindType;
  return {
    // @Todo implement
    ["FILER_ID"]: committee.efsFilerId,
    // @Todo implement
    ["FILING_PERIOD_ID"]: filingDates[2].id,
    ["FILING_CAT_ID"]: 1,
    ["ELECT_ID"]: committee.efsElectionId,
    ["RESIG_TERM_TYPE_ID"]: "NULL",
    // @Todo implement
    ["R_FILING_DATE"]: millisToDateStr(filingDates[2].filingDate),
    ["FILING_SCHED_ID"]: getFilingScheduleId(txn),
    ["LOAN_LIB_NUMBER"]: "NULL",
    ["TRANS_NUMBER"]: transactionId,
    ["TRANS_MAPPING"]: "NULL",
    // @Todo implement
    ["SCHED_DATE"]: millisToDateStr(txn.paymentDate),
    ["ORG_DATE"]: "NULL",
    ["CNTRBR_TYPE_ID"]:
      txn.transactionType === TransactionType.Contribution
        ? NYSEntityTypeId.get(entityType)
        : "NULL",
    // @ToDo add in-kind field
    ["CNTRBN_TYPE_ID"]:
      txn.paymentMethod === PaymentMethod.InKind
        ? NYSInKindTypeId.get(inKindType)
        : "NULL",
    ["TRANSFER_TYPE_ID"]: "NULL",
    ["RECEIPT_TYPE_ID"]: "NULL",
    ["RECEIPT_CODE_ID"]: "NULL",
    ["PURPOSE_CODE_ID"]:
      transactionType === TransactionType.Disbursement
        ? NYSPurposeCodeId.get(purposeCode)
        : "NULL",
    ["Is Expenditure Subcontracted?"]:
      transactionType == TransactionType.Disbursement
        ? boolToYesNo(isSubcontracted)
        : "NULL",
    ["Is Expenditure a Partial Payment?"]:
      transactionType == TransactionType.Disbursement
        ? boolToYesNo(isPartialPayment)
        : "NULL",
    ["Is this existing Liability?"]:
      transactionType == TransactionType.Disbursement
        ? boolToYesNo(isExistingLiability)
        : "NULL",
    ["Is Liability a Partial Forgiven?"]: "NULL",
    ["FLNG_ENT_NAME"]: !isUnItemized(txn)
      ? getEntityName(entityType, firstName, lastName, entityName)
      : "NULL",
    ["FLNG_ENT_FIRST_NAME"]:
      isPerson(entityType) && !isUnItemized(txn) ? firstName : "NULL",
    ["FLNG_ENT_MIDDLE_NAME"]: "NULL",
    ["FLNG_ENT_LAST_NAME"]:
      isPerson(entityType) && !isUnItemized(txn) ? lastName : "NULL",
    ["FLNG_ENT_ADD1"]: !isUnItemized(txn) ? addressLine1 : "NULL",
    ["FLNG_ENT_CITY"]: !isUnItemized(txn) ? city : "NULL",
    ["FLNG_ENT_STATE"]: !isUnItemized(txn) ? state : "NULL",
    ["FLNG_ENT_ZIP"]: !isUnItemized(txn) ? postalCode : "NULL",
    ["FLNG_ENT_COUNTRY"]: !isUnItemized(txn) ? "United States" : "NULL",
    ["PAYMENT_TYPE_ID"]: NYSPaymentTypeId.get(paymentMethod),
    ["PAY_NUMBER"]: txnToPayNumber(txn),
    ["OWED_AMT"]: "NULL",
    ["ORG_AMT"]: centsToDollars(amount),
    ["TRANS_EXPLNTN"]: txnToMemo(txn),
    ["LOAN_OTHER_ID"]: "NULL",
    ["R_ITEMIZED"]: !isUnItemized(txn) ? "Y" : "N",
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
};

const isUnItemized = (txn: ITransaction) => false;

interface FilingPeriod {
  id: number;
  cutOffDate: number;
  filingDate: number;
  scopes: string[];
  state: string;
  race: string;
  desc: string;
}

const millisToDateStr = (millis: number): string => {
  const date = new Date(millis);

  return `${date.getMonth() + 1}/${date.getDate()}/${date.getFullYear()}`;
};

const filingDates: FilingPeriod[] = [
  {
    id: 682,
    desc: "32-Day Pre-General",
    scopes: ["local", "state"],
    state: "ny",
    race: "general",
    cutOffDate: new Date("September 27, 2021").getTime(),
    filingDate: new Date("October 01, 2021").getTime(),
  },
  {
    id: 683,
    desc: "11 Day Pre-General",
    scopes: ["local", "state"],
    state: "ny",
    race: "general",
    cutOffDate: new Date("October 18, 2021").getTime(),
    filingDate: new Date("October 22, 2021").getTime(),
  },
  {
    id: 684,
    desc: "27-Day Post-General",
    scopes: ["local", "state"],
    state: "ny",
    race: "general",
    cutOffDate: new Date("November 25, 2021").getTime(),
    filingDate: new Date("November 29, 2021").getTime(),
  },
];

const getFilingPeriod = (committee: ICommittee): FilingPeriod => {
  const currentTime = now();
  const fps = filingDates.filter(
    (fp) => currentTime < fp.cutOffDate && fp.scopes.includes(committee.scope)
  );
  if (fps.length === 0)
    throw new ApplicationError("Filing period not found", {});
  return fps[0];
};

const txnToPayNumber = (txn: ITransaction): string => {
  txn.paymentMethod === PaymentMethod.Check ? txn.checkNumber : "NULL";

  switch (txn.paymentMethod) {
    case PaymentMethod.Check:
      return `CHK${txn.checkNumber}`;
    case PaymentMethod.Ach:
      return "ACH";
    case PaymentMethod.Wire:
      return "WXF";
    default:
      return "NULL";
  }
};

const txnToMemo = (txn: ITransaction): string => {
  switch (txn.paymentMethod) {
    case PaymentMethod.InKind:
      return txn.inKindDescription;
    default:
      if (txn.purposeCode === PurposeCode.OTHER) {
        return txn.explanation || "NULL";
      }
      return "NULL";
  }
};

export const NYSEntityTypeId = new Map<EntityType, number>([
  [EntityType.Can, 1],
  [EntityType.Fam, 2],
  [EntityType.Ind, 3],
  [EntityType.Solep, 4],
  [EntityType.Part, 5],
  [EntityType.Corp, 6],
  [EntityType.Union, 9],
  [EntityType.Assoc, 10],
  [EntityType.Llc, 11],
  [EntityType.Pac, 12],
  [EntityType.Plc, 13],
  [EntityType.Oth, 14],
]);

export const NYSPurposeCodeId = new Map<PurposeCode, number>([
  [PurposeCode.CMAIL, 1],
  [PurposeCode.CONSL, 2],
  [PurposeCode.CONSV, 3],
  [PurposeCode.CNTRB, 4],
  [PurposeCode.FUNDR, 5],
  [PurposeCode.LITER, 6],
  [PurposeCode.OFFICE, 7],
  [PurposeCode.OTHER, 8],
  [PurposeCode.PETIT, 9],
  [PurposeCode.INT, 10],
  [PurposeCode.REIMB, 11],
  [PurposeCode.POLLS, 13],
  [PurposeCode.POSTA, 14],
  [PurposeCode.PRINT, 15],
  [PurposeCode.PROFL, 16],
  [PurposeCode.RADIO, 17],
  [PurposeCode.RENTO, 18],
  [PurposeCode.TVADS, 19],
  [PurposeCode.VOTER, 20],
  [PurposeCode.WAGES, 21],
  [PurposeCode.BKFEE, 22],
  [PurposeCode.LWNSN, 23],
  [PurposeCode.UTILS, 24],
  [PurposeCode.CCP, 29],
  [PurposeCode.BKKP, 31],
  [PurposeCode.CAR, 32],
  [PurposeCode.CARSVC, 33],
  [PurposeCode.CELL, 34],
  [PurposeCode.EADS, 35],
  [PurposeCode.EMAIL, 36],
  [PurposeCode.GAS, 37],
  [PurposeCode.LODG, 38],
  [PurposeCode.MEALS, 40],
  [PurposeCode.MLGE, 41],
  [PurposeCode.MTG, 42],
  [PurposeCode.PARK, 43],
  [PurposeCode.TOLLS, 45],
  [PurposeCode.XPORT, 46],
  [PurposeCode.BLBD, 47],
  [PurposeCode.WAGE, 48],
  [PurposeCode.NPD, 49],
  [PurposeCode.PIDA, 50],
]);

export const NYSInKindTypeId = new Map<InKindType, number>([
  [InKindType.ServicesFacilitiesProvided, 1],
  [InKindType.PropertyGiven, 2],
  [InKindType.CampaignExpensesPaid, 3],
]);

export const NYSPaymentTypeId = new Map<PaymentMethod, number>([
  [PaymentMethod.Check, 1],
  [PaymentMethod.Ach, 1],
  [PaymentMethod.OnlineProcessor, 4],
  [PaymentMethod.Credit, 2],
  [PaymentMethod.Debit, 3],
  [PaymentMethod.Wire, 5],
  [PaymentMethod.Cash, 6],
  [PaymentMethod.Other, 7],
]);

const isPerson = (entityType: EntityType) =>
  [EntityType.Ind, EntityType.Fam, EntityType.Can].includes(entityType);

const isNonEntityPerson = (entityType: EntityType) =>
  [EntityType.Ind, EntityType.Fam, EntityType.Can].includes(entityType);

const sanitize = (str: string) => str.split(".").join("").split(",").join("");

const getEntityName = (
  entityType: EntityType,
  firstName: string,
  lastName: string,
  companyName?: string
) => {
  if (isNonEntityPerson(entityType)) {
    return `${firstName} ${lastName}`;
  } else {
    return sanitize(companyName);
  }
};

const getFilingScheduleId = (txn: ITransaction): number => {
  if (txn.transactionType === TransactionType.Contribution) {
    if (txn.paymentMethod === PaymentMethod.InKind) {
      return 4;
    } else {
      switch (txn.entityType) {
        case EntityType.Ind:
        case EntityType.Part:
          return 1;
        case EntityType.Corp:
          return 2;
        // case EntityType.Llc:
        //   return 15;
        default:
          return 3;
      }
    }
  } else {
    // Expenditure
    return 6;
  }
};

export const centsToDollars = (amount: number): string => {
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
  ["CNTRBR_TYPE_ID"]: number | string;
  ["CNTRBN_TYPE_ID"]: number | string;
  ["TRANSFER_TYPE_ID"]: string;
  ["RECEIPT_TYPE_ID"]: string;
  ["RECEIPT_CODE_ID"]: string;
  ["PURPOSE_CODE_ID"]: any;
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

// @ToDo Figure out proper place to document / archive this code

export enum Field {
  DONOR_FULL_NAME = "donor_full_name",
  DONOR_ADDRESS = "donor_address",
  ENTITY_NAME = "entity_name",
  EMPLOYER_NAME = "employer_name",
  CPF_ID = "cpf_id",
  PRINCIPAL_OFFICER_FULL_NAME = "principal_officer_full_name",
}

const scheduleAFields = [Field.DONOR_FULL_NAME, Field.DONOR_ADDRESS];

const scheduleBandCFields = [Field.ENTITY_NAME, Field.DONOR_ADDRESS];

export const NYSEntityTypeFields = new Map<string, Field[]>([
  [EntityType.Can, scheduleBandCFields],
  [EntityType.Fam, scheduleAFields],
  [EntityType.Ind, scheduleAFields],
  [EntityType.Solep, scheduleBandCFields],
  [EntityType.Part, scheduleBandCFields],
  [EntityType.Corp, scheduleBandCFields],
  [EntityType.Comm, scheduleBandCFields],
  [EntityType.Union, scheduleBandCFields],
  [EntityType.Assoc, scheduleBandCFields],
  [EntityType.Llc, scheduleBandCFields],
  [EntityType.Pac, scheduleBandCFields],
  [EntityType.Plc, scheduleBandCFields],
  [EntityType.Oth, scheduleBandCFields],
]);

export const NYSEntityTypeAggregateDuration = new Map<
  string,
  AggregateDuration
>([
  [EntityType.Can, AggregateDuration.AGGREGATE_LIMIT],
  [EntityType.Fam, AggregateDuration.AGGREGATE_LIMIT],
  [EntityType.Ind, AggregateDuration.AGGREGATE_LIMIT],
  [EntityType.Solep, AggregateDuration.AGGREGATE_LIMIT],
  [EntityType.Part, AggregateDuration.CALENDAR_YEAR],
  [EntityType.Corp, AggregateDuration.AGGREGATE_LIMIT],
  [EntityType.Comm, AggregateDuration.AGGREGATE_LIMIT],
  [EntityType.Union, AggregateDuration.AGGREGATE_LIMIT],
  [EntityType.Assoc, AggregateDuration.CALENDAR_YEAR],
  [EntityType.Llc, AggregateDuration.AGGREGATE_LIMIT],
  [EntityType.Pac, AggregateDuration.AGGREGATE_LIMIT],
  [EntityType.Plc, AggregateDuration.AGGREGATE_LIMIT],
]);

export const EntityTypeDescription = new Map<string, string>([
  [EntityType.Can, "Candidate/Candidate Spouse"],
  [EntityType.Fam, "Candidate Family Member"],
  [EntityType.Ind, "Individual"],
  [EntityType.Solep, "Sole Proprietorship"],
  [EntityType.Part, "Partnership, including LLPs"],
  [EntityType.Corp, "Corporation"],
  [EntityType.Comm, "Committee"],
  [EntityType.Union, "Union"],
  [EntityType.Assoc, "Association"],
  [EntityType.Llc, "Professional/Limited Liability Company (PLLC/LLC)"],
  [EntityType.Pac, "Political Action Committee (PAC)"],
  [EntityType.Plc, "Political Committee"],
  [EntityType.Oth, "Other"],
]);

enum OfficeType {
  StateLocal = "StateLocal",
  Village = "Village",
  Federal = "Federal",
}

enum ElectionType {
  General = "General",
  Periodic = "Periodic",
  Special = "Special",
  Primary = "Primary",
  PresidentialPrimary = "PresidentialPrimary",
}
