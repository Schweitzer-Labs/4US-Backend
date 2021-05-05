import config from "./config";
import keys from "./enums";
import { Stripe } from "stripe";
import * as dotenv from "dotenv";
import { ApplicationError } from "./application-error";
import { StatusCodes } from "http-status-codes";
import stripCardInfo from "./strip-card-info";

dotenv.config();

const runenv = process.env.RUNENV;

export interface Contribution {
  stripeAccount: string;
  amount: number;
  cardNumber: string;
  cardExpirationMonth: number;
  cardExpirationYear: number;
  cardCVC: string;
}

export const processPaymentFromContribution = async (
  contribution: Contribution
): Promise<string> => {
  const stripeApiKey = await config.get(runenv, keys.stripeApiKey);

  const {
    stripeAccount,
    amount,
    cardNumber,
    cardExpirationMonth,
    cardExpirationYear,
    cardCVC,
  } = contribution;

  const stripe = new Stripe(stripeApiKey, {
    apiVersion: "2020-08-27",
  });

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
