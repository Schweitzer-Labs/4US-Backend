exports.committeeGraphQLHandler =
  require("./dist/committee-graphql.lambda").default;
exports.policapitalContributeHandler =
  require("./dist/policapital-contribute.lambda").default;
exports.policapitalAnalyticsHandler =
  require("./dist/policapital-analytics.lambda").default;
exports.policapitalOnboardHandler =
  require("./dist/policapital-onboard.lambda").default;
exports.transactionEventDispatcherHandler =
  require("./dist/transaction-event-dispatcher.lambda").default;
exports.policapitalEmailerHandler =
  require("./dist/policapital-emailer.lambda").default;
exports.bankSyncHandler = require("./dist/bank-sync.lambda").default;
exports.stratoSQS = require("./dist/strato-sqs.lambda").default;
