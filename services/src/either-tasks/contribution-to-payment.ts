import { Stripe } from "stripe";
import { StatusCodes } from "http-status-codes";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { stripCardInfo, StrippedContribution } from "../utils/strip-card-info";
import { ApplicationError } from "../utils/application-error";
import { IContribution } from "./event-to-contribution";
import { ICommitteeContribution } from "./contribution-to-committee-contribution";

export interface Payment extends StrippedContribution {
  stripePaymentIntentId: string;
}

export const processPaymentFromCommitteeContribution =
  (stripe: Stripe) =>
  async (committeeContribution: ICommitteeContribution): Promise<Payment> => {
    const { contribution, committee } = committeeContribution;
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
          destination: committee.stripeAccount,
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
      throw new ApplicationError(
        "Payment failed",
        {},
        StatusCodes.UNAUTHORIZED
      );
    }
  };

export const committeeContributionToPayment =
  (stripe: Stripe) =>
  (
    committeeContribution: ICommitteeContribution
  ): TaskEither<ApplicationError, Payment> =>
    tryCatch<ApplicationError, any>(
      () =>
        processPaymentFromCommitteeContribution(stripe)(committeeContribution),
      (error) =>
        new ApplicationError("Payment failed", {}, StatusCodes.UNAUTHORIZED)
    );
