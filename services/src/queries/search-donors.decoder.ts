import { EntityType } from "../utils/enums/entity-type.enum";
import * as t from "io-ts";

export interface IDonorInput {
  firstName: string;
  lastName: string;
  addressLine1: string;
  city: string;
  state: string;
  postalCode: string;
  entityType: EntityType;
  id?: string;
  emailAddress?: string;
  middleName?: string;
  addressLine2?: string;
  employer?: string;
  occupation?: string;
  companyName?: string;
  phoneNumber?: string;
  attestsToBeingAnAdultCitizen?: boolean;
  cardNumberLastFourDigits?: string;
  entityName?: string;
}

const DonorRequired = t.type({
  id: t.string,
  flacspeeMatch: t.string,
  createdTimestamp: t.number,
  firstName: t.string,
  lastName: t.string,
  addressLine1: t.string,
  city: t.string,
  state: t.string,
  postalCode: t.string,
  entityType: t.string,
});

const DonorOptional = t.partial({
  emailAddress: t.string,
  middleName: t.string,
  addressLine2: t.string,
  employer: t.string,
  occupation: t.string,
  companyName: t.string,
  phoneNumber: t.string,
  transactionType: t.string,
  attestsToBeingAnAdultCitizen: t.boolean,
  cardNumberLastFourDigits: t.string,
  entityName: t.string,
  instantIdComprehensiveVerificationScore: t.number,
  instantIdUniqueId: t.string,
  instantIdRawResponse: t.unknown,
  instantIdRequestTimestamp: t.number,
});

export const Donor = t.intersection([DonorRequired, DonorOptional]);
export const Donors = t.array(Donor);

export type IDonor = t.TypeOf<typeof Donor>;
