import { expect } from "chai";

import graphql from "../../src/committee-graphql.lambda";
import { genGraphQLProxy } from "../utils/gen-allowed-proxy.util";
import { putTransaction } from "../../src/utils/model/put-transaction.utils";
import { genContributionRecord } from "../utils/gen-contribution.util";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { sleep } from "../../src/utils/sleep.utils";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;

const validUsername = "evan-piro";

const invalidUsername = "james-martin";

const expectedForbiddenText =
  "Access denied! You need to be authorized to perform this action!";

const committeeId = "pat-miller";

const getAllTransactionsQuery = `
  query {
    transactions(committeeId: "${committeeId}") {
      lastName
      firstName
      amount
      direction
    }
  }
`;

const getTransactionsByDonorIdQuery = (donorId: string) => `
  query {
    transactions(committeeId: "${committeeId}", donorId: "${donorId}") {
      donorId
      lastName
      firstName
      amount
      direction
    }
  }
`;

const getCommitteeQuery = `
  query {
    committee(committeeId: "${committeeId}") {
      id
      candidateFirstName
    }
  }
`;

const aggregationsQuery = `
  query {
    aggregations(committeeId: "${committeeId}") {
      balance,
      totalRaised,
      totalSpent,
      totalDonors,
      totalTransactions,
      totalContributionsInProcessing,
      totalDisbursementsInProcessing,
      needsReviewCount
    }
  }
`;

const lambdaPromise = (lambda, event, context) => {
  return new Promise((resolve, reject) => {
    lambda(event, context, (error, res) => {
      resolve(res);
    });
  });
};

describe("Committee GraphQL Lambda", function () {
  describe("Permissions", function () {
    it("Prevents a non-member user from querying a committee", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getCommitteeQuery, invalidUsername),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.errors[0].message).to.equal(expectedForbiddenText);
    });
    it("Prevents a non-member user from querying a transaction", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getAllTransactionsQuery, invalidUsername),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.errors[0].message).to.equal(expectedForbiddenText);
    });
    it("Prevents a non-member user from querying an aggregation", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(aggregationsQuery, invalidUsername),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.errors[0].message).to.equal(expectedForbiddenText);
    });
  });
  describe("Transactions", function () {
    it("Get by Committee ID", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getAllTransactionsQuery, validUsername),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.data.transactions.length > 0).to.equal(true);
    });
    it("Get by Committee ID and Donor ID", async () => {
      const donorId = genTxnId();
      const txn = genContributionRecord(committeeId, donorId);

      await putTransaction(txnsTableName)(dynamoDB)(txn);

      await sleep(1000);

      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getTransactionsByDonorIdQuery(donorId), validUsername),
        {}
      );
      const body: any = JSON.parse(res.body);
      expect(body.data.transactions[0].donorId).to.equal(donorId);
    });
  });
  describe("Committee", function () {
    it("Get by Committee ID", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getCommitteeQuery, validUsername),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.data.committee.id).to.equal("pat-miller");
    });
  });
  describe("Aggregations", function () {
    it("Get by Committee ID", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(aggregationsQuery, validUsername),
        {}
      );

      const body = JSON.parse(res.body);
      expect(body.data.aggregations.balance).to.be.a("number");
    });
  });
});
