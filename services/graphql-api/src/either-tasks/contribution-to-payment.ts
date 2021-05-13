import { Stripe } from "stripe";
import { StatusCodes } from "http-status-codes";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { stripCardInfo } from "../utils/strip-card-info";
import { ApplicationError } from "../utils/application-error";

export interface Contribution {
  stripeAccount: string;
  amount: number;
  cardNumber: string;
  cardExpirationMonth: number;
  cardExpirationYear: number;
  cardCVC: string;
}

export const processPaymentFromContribution = (stripe: Stripe) => async (
  contribution: Contribution
): Promise<any> => {
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

    console.log("Payment succeeded", {
      ...stripCardInfo(contribution),
      stripePaymentIntentId,
    });

    return stripePaymentIntentId;
  } catch (e) {
    console.error("Payment failed", stripCardInfo(contribution));
    throw new ApplicationError("Payment failed", StatusCodes.UNAUTHORIZED);
  }
};

export const contributionToPayment = (stripe: Stripe) => (
  contribution: Contribution
): TaskEither<ApplicationError, object> =>
  tryCatch<ApplicationError, any>(
    () => processPaymentFromContribution(stripe)(contribution),
    (error) => new ApplicationError("Payment failed", StatusCodes.UNAUTHORIZED)
  );
