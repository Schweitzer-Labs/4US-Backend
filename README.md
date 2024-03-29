# 4US Backend

## Description
This project provides the means for developing and maintaining 4US data services.

## Project Structure
[`cfn/`](cfn/): Contains CloudFormation configuration templates which are used to provision application infrastructure such as API Gateways, Lambda functions, SQS queues, S3 buckets, and DynamoDB tables. Once a file is added or updated and pushed to QA, Demo, or Prod, a continuous integration process will compile the files into a CloudFormation template and deploy them to their respective environments.

[`.github/`](.github/): Contains the configuration for the GitHub action which is responsible for providing continuous integration to AWS.

[`lambdas/`](lambdas): Provides the [ServicesLayer](cfn/templates/backend/07_Resources/Lambda/Layer/ServicesLayer.yml) layer code which our `services/` project builds into. This directory is rarely ever touched as it only serves as the glue between the Lambda Layer configuration and the services source code.

[`services/`](services/): This is where most of 4US daily backend development happens. It contains the source code of all of our lambdas and exposes the lambdas via the [`services/app.js`](services/app.js) file, which references the compiled JS files in the `services/dist` directory generated by the TypeScript compiler. To start diving into development, take a look at the directory's [README](./services/README.md).

[`Makefile`](Makefile): Provides the deployment script for the GitHub action.

[`committee-redirector/`](./committee-redirector): To be removed.
