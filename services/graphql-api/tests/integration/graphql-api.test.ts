import { expect } from "chai";
import graphql from "../../src/graphql-api";
import {
  validGraphQLProxyContext,
  validGraphQLProxyEvent,
} from "../events/valid-graphql-proxy";

const lambdaPromise = (lambda, event, context) => {
  return new Promise((resolve, reject) => {
    lambda(event, context, (error, res) => {
      resolve(res);
    });
  });
};

describe("GraphQL Endpoint Lambda", function () {
  it("Accepts a valid graphQL query", async () => {
    const res: any = await lambdaPromise(
      graphql,
      validGraphQLProxyEvent,
      validGraphQLProxyContext
    );
    const body = JSON.parse(res.body);
    expect(body.data.transactions.length).to.equal(0);
  });
});
