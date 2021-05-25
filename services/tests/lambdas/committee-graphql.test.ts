import { expect } from "chai";
import graphql from "../../src/committee-graphql.lambda";
import { forbiddenGraphqlMemberProxy } from "../events/forbidden-graphql-member.proxy";
import { genGraphQLProxy } from "../utils/gen-allowed-proxy.util";

const expectedForbiddenText =
  "Access denied! You need to be authorized to perform this action!";

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
  describe("Permissions", function () {
    it("Prevents a non-member user from querying a committee", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(
          getCommitteeQuery,
          "9ee487a4-f767-4298-9cb8-ce73652f96fd"
        ),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.errors[0].message).to.equal(expectedForbiddenText);
    });
    it("Prevents a non-member user from querying a transaction", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(
          getAllTransactionsQuery,
          "9ee487a4-f767-4298-9cb8-ce73652f96fd"
        ),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.errors[0].message).to.equal(expectedForbiddenText);
    });
    it("Prevents a non-member user from querying an aggregation", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(
          aggregationsQuery,
          "9ee487a4-f767-4298-9cb8-ce73652f96fd"
        ),
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
        genGraphQLProxy(
          getAllTransactionsQuery,
          "36fcc915-a3d2-4bba-997d-281c46419974"
        ),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.data.transactions.length > 0).to.equal(true);
    });
  });
  describe("Committee", function () {
    it("Get by Committee ID", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(
          getCommitteeQuery,
          "36fcc915-a3d2-4bba-997d-281c46419974"
        ),
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
        genGraphQLProxy(
          aggregationsQuery,
          "36fcc915-a3d2-4bba-997d-281c46419974"
        ),
        {}
      );

      const body = JSON.parse(res.body);
      expect(body.data.aggregations.balance).to.be.a("number");
    });
  });
});
