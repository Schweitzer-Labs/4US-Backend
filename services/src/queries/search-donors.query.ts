export const searchDonors = () => {};

interface Donor {
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
