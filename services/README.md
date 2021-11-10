# 4US Services Layer
## About the Codebase
This codebase is a TypeScript project that allows developers to create and maintain Lambdas and share dependencies between them. It also provides a framework for testing modules.

## The Lambdas
The following Lambdas, as seen in the [src/](./src) directory provide the entry points for all data services on our platform.

- Bank Sync Lambda: Queues committees for bank data synchronization. Runs at a configuration interval (5 minutes).
- Bank SQS Lambda: Processes items in the bank data synchronization queue, making requests to our bank data providers and ensuring the committee's transaction data is up to date.
- 

