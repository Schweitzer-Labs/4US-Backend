# 4US Services Layer
## About the Codebase
This codebase is a TypeScript project that allows developers to create and maintain Lambdas and share dependencies between them. It also provides a framework for testing modules.

## The Lambdas
The following Lambdas, as seen in the [src/](./src) directory provide the entry points for all data services on our platform. To view the CloudFormation configuration of these lambdas, go to [their resource definitions](../cfn/templates/backend/07_Resources/Lambda/Function).

### The Core Services

[`Committee GraphQL Lambda`](./src/committee-graphql.lambda.ts): Exposes an authenticated GraphQL API for Committees to query and mutate data on the platform. The primary use of this API is from the committee dashboard.

[`Platform Contribute Lambda`](./src/platform-contribute.lambda.ts): Handles an HTTP post request for processing a contribution. This is used primarily from our committees public facing donation pages.

[`Transaction Event Dispatcher Lambda`](./src/transaction-event-dispatcher.lambda.ts): Triggered by state changes to the Transactions DynamoDB table. This function is responsible for pattern matching against updates and applying operations to address specific states. These operations consist of queuing notification emails for sending to donors and committees, updating aggregations for committees to reflect current state (total contributions, total donors, money in flight.etc), and pushing transactions to the StratoQueue for committing to Strato.

[`Bank Sync Lambda`](./src/bank-sync.lambda.ts): Queues committees for bank data synchronization. Runs at a configured interval (currently 5 minutes).

[`Bank SQS Lambda`](./src/bank-sqs.lambda.ts): Processes items in the bank data synchronization queue (BankQueue), making requests to our bank data providers and ensuring the committee's transaction data is up-to-date.

[`Stripe Webhook Lambda`](./src/strato-sqs.lambda.ts): Handles Stripe initiated HTTP requests of events we've subscribed to via the [Stripe webhooks UI](https://dashboard.stripe.com/webhooks). We're currently utilizing Stripe's payout paid event to request a reconciliation report and a report generated event to use the resulting reconciliation report data to reconcile transactions.

[`Emailer Lambda`](./src/policapital-emailer.lambda.ts): Processes items in the EmailQueue by send the mail via SES.

[`Strato SQS Lambda`](./src/strato-sqs.lambda.ts): Processes items in the StratoQueue for committing to Strato via the [Strato API](https://github.com/blockapps/blockapps-rest).

### Miscellaneous Services  

[`Run Report Lambda`](./src/run-report.lambda.ts): Runs a reconciliation report via manual invocation.

[`Onboard Lambda`](./src/policapital-onboard.lambda.ts): Receives an HTTP request with a query string containing a Stripe token and sends it to Stripe to finalize Connect account set up. This function is currently being deprecated.

## Running on Local
Once you've gotten AWS access and have your MFA keys set up, go to [`.sample-env`](./.sample-env), copy it to `.env`, and fill out the values. Ask another developer for some of these values to make things easier.

Now you can run the APIs on local. Run `npm run test-server`. This starts up two express apps. The Committee GraphQL lambda on http://localhost:4000 and the Platform Contribute Lambda on http://localhost:4001. With these processes running, you can now run requests those endpoints.

## Testing
Our testing code is in the [`tests/`](./tests) directory. We use [`chai`](https://github.com/chaijs/chai) and [`ts-mocha`](https://github.com/piotrwitek/ts-mocha). Run `npm test` to run the whole suite or `ts-mocha tests/**/<file pattern of test>* --timeout 600000` to test something specific.

Our approach to testing is pretty flexible. We like to test the whole system, hence the [`tests/lambdas`](./tests/lambdas) directory. And pieces of the system in [`tests/integration`](./tests/integration).







