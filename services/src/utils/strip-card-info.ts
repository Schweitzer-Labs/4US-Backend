import { IContribution } from "../pipes/event-to-contribution";
import { Source } from "./enums/source.enum";

export const stripCardInfo = ({
  cardCVC,
  cardExpirationMonth,
  cardExpirationYear,
  cardNumber,
  ...rest
}: IContribution): StrippedContribution => ({
  ...rest,
  cardNumberLastFourDigits: cardNumber.substr(cardNumber.length - 4),
});

export interface StrippedContribution {
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
  amount: number;
  refCode?: string;
  committeeId: string;
  entityType?: string;
  cardNumberLastFourDigits: string;
  attestsToBeingAdultCitizen?: boolean;
  paymentMethod: string;
  donorId?: string;
  ruleCode?: string;
  source?: Source;
  donorVerificationScore?: number;
}
