import { expect } from "chai";
import graphql from "../../src/graphql-api";
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

const getDisbursementsQuery = `
  query {
    transactions(committeeId: "907b427a-f8a9-450b-9d3c-33d8ec4a4cc4") {
      lastName
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

describe("GraphQL Endpoint Lambda", function () {
  describe("Transactions", function () {
    it("Get all", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getAllTransactionsQuery),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.data.transactions.length > 0).to.equal(true);
    });
    // it("Get contributions", async () => {
    //   const res: any = await lambdaPromise(
    //     graphql,
    //     genGraphQLProxy(getContributionsQuery),
    //     {}
    //   );
    //   const body = JSON.parse(res.body);
    //   expect(body.data.transactions.length > 0).to.equal(true);
    // });
    // it("Get disbursements", async () => {
    //   const res: any = await lambdaPromise(
    //     graphql,
    //     genGraphQLProxy(getDisbursementsQuery),
    //     {}
    //   );
    //   const body = JSON.parse(res.body);
    //   expect(body.data.transactions.length > 0).to.equal(true);
    // });
  });
});
