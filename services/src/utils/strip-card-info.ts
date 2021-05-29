import { IContribution } from "../either-tasks/event-to-contribution";

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
  email: string;
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
  committee?: string;
  entityType?: string;
  cardNumberLastFourDigits: string;
  attestsToBeingAdultCitizen?: boolean;
  paymentMethod: string;
}
