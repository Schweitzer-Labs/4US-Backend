import { Source } from "../utils/enums/source.enum";

export interface IContribution {
  amount: number;
  cardNumber?: string;
  cardExpirationMonth?: number;
  cardExpirationYear?: number;
  cardCVC?: string;
  paymentMethod: string;
  cardNumberLastFourDigits?: string;
  firstName?: string;
  lastName?: string;
  email?: string;
  occupation?: string;
  employer?: string;
  addressLine1?: string;
  addressLine2?: string;
  city?: string;
  state?: string;
  postalCode?: string;
  phoneNumber?: string;
  refCode?: string;
  committeeId: string;
  entityType?: string;
  donorId?: string;
  ruleCode?: string;
  source?: Source;
  createdByUser?: string;
  donorVerificationScore?: number;
}
