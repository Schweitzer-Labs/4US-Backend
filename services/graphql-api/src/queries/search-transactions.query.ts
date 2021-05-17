export const searchTransactions = () => {};

interface Transaction {
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
  attestsToBeingAnAdultCitizen?: string;
}
