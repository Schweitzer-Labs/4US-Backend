import { Stripe } from "stripe";
import { StatusCodes } from "http-status-codes";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { stripCardInfo, StrippedContribution } from "../utils/strip-card-info";
import { ApplicationError } from "../utils/application-error";

export interface Contribution {
  stripeAccount: string;
  amount: number;
  cardNumber: string;
  cardExpirationMonth: number;
  cardExpirationYear: number;
  cardCVC: string;
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
  refCode?: string;
  committee?: string;
  contributorType?: string;
  paymentMethod?: string;
}

export interface Payment extends StrippedContribution {
  stripePaymentIntentId: string;
}

export const processPaymentFromContribution =
  (stripe: Stripe) =>
  async (contribution: Contribution): Promise<Payment> => {
    const {
      stripeAccount,
      amount,
      cardNumber,
      cardExpirationMonth,
      cardExpirationYear,
      cardCVC,
    } = contribution;

    try {
      const methodRes = await stripe.paymentMethods.create({
        type: "card",
        card: {
          number: cardNumber,
          exp_month: cardExpirationMonth,
          exp_year: cardExpirationYear,
          cvc: cardCVC,
        },
      });

      const paymentMethodId = methodRes.id;

      const res = await stripe.paymentIntents.create({
        amount,
        currency: "usd",
        payment_method: paymentMethodId,
        confirm: true,
        transfer_data: {
          destination: stripeAccount,
        },
      });

      const { id: stripePaymentIntentId } = res;

      const payment = {
        ...stripCardInfo(contribution),
        stripePaymentIntentId,
      };

      console.log("Payment succeeded", payment);

      return payment;
    } catch (e) {
      console.error("Payment failed", stripCardInfo(contribution));
      throw new ApplicationError("Payment failed", StatusCodes.UNAUTHORIZED);
    }
  };

export const contributionToPayment =
  (stripe: Stripe) =>
  (contribution: Contribution): TaskEither<ApplicationError, Payment> =>
    tryCatch<ApplicationError, any>(
      () => processPaymentFromContribution(stripe)(contribution),
      (error) =>
        new ApplicationError("Payment failed", StatusCodes.UNAUTHORIZED, error)
    );
