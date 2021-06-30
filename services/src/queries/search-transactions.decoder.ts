import * as t from "io-ts";
import { FinicityTransaction } from "../clients/finicity/finicity.decoders";

const TransactionRequired = t.type({
  id: t.string,
  committeeId: t.string,
  direction: t.string,
  amount: t.number,
  paymentMethod: t.string,
  bankVerified: t.boolean,
  ruleVerified: t.boolean,
  initiatedTimestamp: t.number,
  source: t.string,
});

const TransactionOptional = t.partial({
  donorId: t.string,
  paymentDate: t.number,
  checkNumber: t.string,
  bankVerifiedTimestamp: t.number,
  ruleVerifiedTimestamp: t.number,
  purposeCode: t.string,
  refCode: t.string,
  // Donor Props
  firstName: t.string,
  middleName: t.string,
  lastName: t.string,
  addressLine1: t.string,
  addressLine2: t.string,
  city: t.string,
  state: t.string,
  postalCode: t.string,
  employer: t.string,
  occupation: t.string,
  entityType: t.string,
  companyName: t.string,
  phoneNumber: t.string,
  emailAddress: t.string,
  transactionType: t.string,
  attestsToBeingAnAdultCitizen: t.boolean,
  stripePaymentIntentId: t.string,
  stripeBalanceTransactionId: t.string,
  stripeChargeId: t.string,
  stripeTransferId: t.string,
  stripePaymentId: t.string,
  stripePayoutId: t.string,
  stripeAutomaticPayoutEffectiveAtUtc: t.number,
  cardNumberLastFourDigits: t.string,
  entityName: t.string,
  ruleCode: t.string,
  createdByUser: t.string,
  isSubcontracted: t.boolean,
  isPartialPayment: t.boolean,
  isExistingLiability: t.boolean,
  blockchainMetadata: t.unknown,
  finicityTransactionId: t.number,
  finicityTransactionData: FinicityTransaction,
  employmentStatus: t.string,
});

export const Transaction = t.intersection([
  TransactionRequired,
  TransactionOptional,
]);

export const Transactions = t.array(Transaction);

export type ITransaction = t.TypeOf<typeof Transaction>;
