import * as t from "io-ts";
import { FinicityTransaction } from "../clients/finicity/finicity.decoders";
import { OwnerSchema } from "../graphql/types/transaction.type";

export enum SchemaVersion {
  V1 = "v1",
}

export const currentVersion = SchemaVersion.V1;

export const OwnerRequired = t.type({
  firstName: t.string,
  lastName: t.string,
  addressLine1: t.string,
  city: t.string,
  state: t.string,
  postalCode: t.string,
  percentOwnership: t.string,
});

export const OwnerPartial = t.partial({
  addressLine2: t.string,
  attributedAmount: t.number,
});

export const Owner = t.intersection([OwnerRequired, OwnerPartial]);

export type IOwner = t.TypeOf<typeof Owner>;

export const Owners = t.array(Owner);

const TransactionRequired = t.type({
  id: t.string,
  committeeId: t.string,
  direction: t.string,
  amount: t.number,
  paymentMethod: t.string,
  bankVerified: t.boolean,
  ruleVerified: t.boolean,
  initiatedTimestamp: t.number,
  paymentDate: t.number,
  source: t.string,
});

const TransactionOptional = t.partial({
  donorId: t.string,
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
  // @Todo remove companyName
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
  modifiedByUser: t.string,
  isSubcontracted: t.boolean,
  isPartialPayment: t.boolean,
  isExistingLiability: t.boolean,
  blockchainMetadata: t.unknown,
  finicityTransactionId: t.number,
  finicityTransactionData: FinicityTransaction,
  finicityNormalizedPayeeName: t.string,
  finicityCategory: t.string,
  finicityBestRepresentation: t.string,
  finicityDescription: t.string,
  finicityTransactionDate: t.number,
  finicityPostedDate: t.number,
  finicityPaymentMethod: t.string,
  employmentStatus: t.string,
  donorVerificationScore: t.number,
  businessIdVerificationScore: t.unknown,
  businessIdRequestTimestamp: t.number,
  businessIdRawResponse: t.unknown,
  inKindType: t.string,
  inKindDescription: t.string,
  owners: Owners,
  explanation: t.string,
});

export const Transaction = t.intersection([
  TransactionRequired,
  TransactionOptional,
]);

export const Transactions = t.array(Transaction);

export type ITransaction = t.TypeOf<typeof Transaction>;
