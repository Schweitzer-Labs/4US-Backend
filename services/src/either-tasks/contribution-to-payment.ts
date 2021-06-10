import { Stripe } from "stripe";
import { StatusCodes } from "http-status-codes";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { stripCardInfo, StrippedContribution } from "../utils/strip-card-info";
import { ApplicationError } from "../utils/application-error";
import { ICommitteeContribution } from "./contribution-to-committee-contribution";
import { Plan } from "../utils/enums/plan.enum";

export interface Payment extends StrippedContribution {
  stripePaymentIntentId: string;
  ruleVerified: boolean;
}

export const processPaymentFromCommitteeContribution =
  (stripe: Stripe) =>
  async (committeeContribution: ICommitteeContribution): Promise<Payment> => {
    const { contribution, committee } = committeeContribution;
    const {
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

      const payment: Payment = {
        ...stripCardInfo(contribution),
        stripePaymentIntentId,
        ruleVerified: committee.platformPlan === Plan.FourUs,
      };

      console.log("Payment succeeded", payment);

      return payment;
    } catch (e) {
      console.error("Payment failed", stripCardInfo(contribution));
      throw new ApplicationError(
        "Payment failed. Please ensure your card info is correct.",
        e,
        StatusCodes.UNPROCESSABLE_ENTITY
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
        new ApplicationError(
          "Payment failed. Please ensure your card info is correct.",
          error,
          StatusCodes.UNPROCESSABLE_ENTITY
        )
    );
