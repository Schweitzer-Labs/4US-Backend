import { Stripe } from "stripe";
import { getStripeMetadata } from "./pipes/contribution-to-payment";
import { unverifiedContributionsData } from "../tests/seed/unverified-contributions.data";
import { sleep } from "./utils/sleep.utils";

const apiKey =
  "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";
const stripe = new Stripe(apiKey, {
  apiVersion: "2020-08-27",
});

const committeeAccount = "acct_1IQFp0RNcAftf9zR";

const pullData = async (id: string) => {
  const res = await stripe.paymentIntents.retrieve(id);
  const metaRes = await getStripeMetadata(stripe)(res);
  return metaRes;
};

// pullData("pi_1J0uqZEUhH8cxK5gAuXCzJaB");

const runAll = async () => {
  const txns = [];
  for (const txn of unverifiedContributionsData) {
    await sleep(100);
    const metaRes = await pullData(txn.stripePaymentIntentId);
    txns.push({
      ...txn,
      ...metaRes,
    });
  }

  console.log(txns);
};

runAll();
