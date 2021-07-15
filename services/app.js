exports.committeeGraphQLHandler =
  require("./dist/committee-graphql.lambda").default;
exports.policapitalOnboardHandler =
  require("./dist/policapital-onboard.lambda").default;
exports.transactionEventDispatcherHandler =
  require("./dist/transaction-event-dispatcher.lambda").default;
exports.policapitalEmailerHandler =
  require("./dist/policapital-emailer.lambda").default;
exports.bankSyncHandler = require("./dist/bank-sync.lambda").default;
exports.platformContribute =
  require("./dist/platform-contribute.lambda").default;
exports.stripeWebhook = require("./dist/stripe-webhook.lambda").default;
