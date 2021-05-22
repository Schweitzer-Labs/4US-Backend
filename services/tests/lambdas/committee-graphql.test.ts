import { expect } from "chai";
import graphql from "../../src/committee-graphql.lambda";
import { genGraphQLProxy } from "../utils/gen-proxy.util";

const getAllTransactionsQuery = `
  query {
    transactions(committeeId: "907b427a-f8a9-450b-9d3c-33d8ec4a4cc4") {
      lastName
      firstName
      amount
      direction
    }
  }
`;

const getContributionsQuery = `
  query {
    transactions(committeeId: "907b427a-f8a9-450b-9d3c-33d8ec4a4cc4") {
      lastName
    }
  }
`;

const getCommitteeQuery = `
  query {
    committee(committeeId: "907b427a-f8a9-450b-9d3c-33d8ec4a4cc4") {
      id
      candidateFirstName
    }
  }
`;

const aggregationsQuery = `
  query {
    aggregations(committeeId: "907b427a-f8a9-450b-9d3c-33d8ec4a4cc4") {
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
  describe("Transactions", function () {
    it("Get by Committee ID", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getAllTransactionsQuery),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.data.transactions.length === 100).to.equal(true);
    });
  });
  describe("Committee", function () {
    it("Get by Committee ID", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getCommitteeQuery),
        {}
      );

      const body = JSON.parse(res.body);
      console.log(body);
      expect(body.data.committee.id).to.equal(
        "907b427a-f8a9-450b-9d3c-33d8ec4a4cc4"
      );
    });
  });
  describe("Aggregations", function () {
    it("Get by Committee ID", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(aggregationsQuery),
        {}
      );

      const body = JSON.parse(res.body);
      console.log(body);
      expect(body.data.aggregations.id).to.equal(
        "907b427a-f8a9-450b-9d3c-33d8ec4a4cc4"
      );
    });
  });
});
