import { Stripe } from "stripe";
import { StatusCodes } from "http-status-codes";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { stripCardInfo, StrippedContribution } from "../utils/strip-card-info";
import { ApplicationError } from "../utils/application-error";
import { ICommitteeContribution } from "./contribution-to-committee-contribution";
import { Plan } from "../utils/enums/plan.enum";

export interface Payment extends StrippedContribution {
  stripePaymentIntentId?: string;
  stripeBalanceTransactionId?: string;
  stripeChargeId?: string;
  ruleVerified: boolean;
}

export const processPaymentFromCommitteeContribution =
  (stripe: Stripe) =>
  async (committeeContribution: ICommitteeContribution): Promise<Payment> => {
    console.log("Payment processing", JSON.stringify(committeeContribution));
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

      console.log(JSON.stringify(res));

      console.log(res.charges.data);

      const stripeMetadata = await getStripeMetadata(stripe)(res);

      const payment: Payment = {
        ...stripCardInfo(contribution),
        ...stripeMetadata,
        ruleVerified: committee.platformPlan === Plan.FourUs,
      };

      console.log("Payment succeeded", JSON.stringify(payment));

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

interface GetStripeMetadataRes {
  stripeBalanceTransactionId: string;
  stripeChargeId: string;
  stripePaymentIntentId: string;
  stripeTransferId: string;
  stripePaymentId: string;
}

export const getStripeMetadata =
  (stripe: Stripe) =>
  async (
    res: Stripe.Response<Stripe.PaymentIntent>
  ): Promise<GetStripeMetadataRes | {}> => {
    const chargeData = res?.charges?.data;
    if (chargeData?.length > 0) {
      const transferId: any = chargeData[0].transfer;

      const transferData = await stripe.transfers.retrieve(transferId);

      console.log("transfer data", transferData);

      return {
        stripeBalanceTransactionId: chargeData[0].balance_transaction,
        stripeChargeId: chargeData[0].id,
        stripePaymentIntentId: res.id,
        stripeTransferId: transferId,
        stripePaymentId: transferData.destination_payment,
      };
    } else {
      return {
        stripePaymentIntentId: res.id,
      };
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
