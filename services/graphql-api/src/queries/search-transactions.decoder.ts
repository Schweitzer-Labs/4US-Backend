import * as t from "io-ts";
import {
  ddbBool,
  ddbNumber,
  ddbString,
  extractDDBBool,
  extractDDBNumber,
  extractDDBString,
} from "../repositories/ddb.utils";
import { right, TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";

const DDBTransactionRequired = t.type({
  amount: ddbNumber,
  paymentMethod: ddbString,
  bankVerified: ddbBool,
  ruleVerified: ddbBool,
  initiatedTimestamp: ddbString,
});

const DDBTransactionOptional = t.partial({
  bankVerifiedTimestamp: ddbString,
  ruleVerifiedTimestamp: ddbString,
  purposeCode: ddbString,
  refCode: ddbString,
  // Donor Props
  firstName: ddbString,
  middleName: ddbString,
  lastName: ddbString,
  addressLine1: ddbString,
  addressLine2: ddbString,
  city: ddbString,
  state: ddbString,
  postalCode: ddbString,
  employer: ddbString,
  occupation: ddbString,
  entityType: ddbString,
  companyName: ddbString,
  phoneNumber: ddbString,
  emailAddress: ddbString,
  attestsToBeingAnAdultCitizen: ddbBool,
});

export const DDBTransaction = t.intersection([
  DDBTransactionRequired,
  DDBTransactionOptional,
]);

export const DDBTransactionsRes = t.type({
  Items: t.array(DDBTransaction),
});

export type DDBTransactionRes = t.TypeOf<typeof DDBTransactionsRes>;

export interface Transaction {
  amount: number;
  paymentMethod: string;
  bankVerified: boolean;
  ruleVerified: boolean;
  initiatedTimestamp: string;
  bankVerifiedTimestamp?: string;
  ruleVerifiedTimestamp?: string;
  purposeCode?: string;
  refCode?: string;
  // Donor Props
  firstName?: string;
  middleName?: string;
  lastName?: string;
  addressLine1: string;
  addressLine2?: string;
  city?: string;
  state?: string;
  postalCode?: string;
  employer?: string;
  occupation?: string;
  entityType?: string;
  companyName?: string;
  phoneNumber?: string;
  emailAddress?: string;
  attestsToBeingAnAdultCitizen?: boolean;
}

export const ddbResponseToTransactions = (
  ddbResponse: DDBTransactionRes
): TaskEither<ApplicationError, Transaction[]> => {
  const transactions = ddbResponse.Items.map((txn) => ({
    amount: extractDDBNumber(txn.amount),
    paymentMethod: extractDDBString(txn.paymentMethod),
    bankVerified: extractDDBBool(txn.bankVerified),
    ruleVerified: extractDDBBool(txn.ruleVerified),
    initiatedTimestamp: extractDDBString(txn.paymentMethod),
    bankVerifiedTimestamp: extractDDBString(txn.bankVerifiedTimestamp),
    ruleVerifiedTimestamp: extractDDBString(txn.ruleVerifiedTimestamp),
    purposeCode: extractDDBString(txn.purposeCode),
    refCode: extractDDBString(txn.refCode),
    firstName: extractDDBString(txn.firstName),
    middleName: extractDDBString(txn.middleName),
    lastName: extractDDBString(txn.lastName),
    addressLine1: extractDDBString(txn.addressLine1),
    addressLine2: extractDDBString(txn.addressLine2),
    city: extractDDBString(txn.city),
    state: extractDDBString(txn.state),
    postalCode: extractDDBString(txn.postalCode),
    employer: extractDDBString(txn.employer),
    occupation: extractDDBString(txn.occupation),
    entityType: extractDDBString(txn.entityType),
    companyName: extractDDBString(txn.companyName),
    phoneNumber: extractDDBString(txn.phoneNumber),
    emailAddress: extractDDBString(txn.emailAddress),
    attestsToBeingAnAdultCitizen: extractDDBBool(
      txn.attestsToBeingAnAdultCitizen
    ),
  }));
  return right(transactions);
};
