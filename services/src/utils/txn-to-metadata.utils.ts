import { ITransaction } from "../model/transaction.type";

export const txnToMetadata = ({
  donorId,
  checkNumber,
  purposeCode,
  firstName,
  middleName,
  lastName,
  addressLine1,
  addressLine2,
  city,
  state,
  postalCode,
  employer,
  occupation,
  entityType,
  phoneNumber,
  emailAddress,
  transactionType,
  attestsToBeingAnAdultCitizen,
  entityName,
  ruleCode,
  isSubcontracted,
  isPartialPayment,
  isExistingLiability,
  employmentStatus,
  donorVerificationScore,
  businessIdVerificationScore,
  inKindType,
  inKindDescription,
  source,
}: ITransaction) => ({
  donorId,
  checkNumber,
  purposeCode,
  firstName,
  middleName,
  lastName,
  addressLine1,
  addressLine2,
  city,
  state,
  postalCode,
  employer,
  occupation,
  entityType,
  phoneNumber,
  emailAddress,
  transactionType,
  attestsToBeingAnAdultCitizen,
  entityName,
  ruleCode,
  isSubcontracted,
  isPartialPayment,
  isExistingLiability,
  employmentStatus,
  donorVerificationScore,
  businessIdVerificationScore,
  inKindType,
  inKindDescription,
  source,
});
