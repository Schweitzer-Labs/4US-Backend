import { Stripe } from "stripe";

const apiKey =
  "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";
const stripe = new Stripe(apiKey, {
  apiVersion: "2020-08-27",
});

// const committeeAccount = "acct_1IqOMPRBYHey0EF7";

const run = async () => {
  // const accounts = await stripe.accounts.list({
  //   limit: 10,
  // });

  const account = await stripe.accounts.retrieve("acct_1JAFqzRToJ9N108i");

  console.log(account);
};

run();
