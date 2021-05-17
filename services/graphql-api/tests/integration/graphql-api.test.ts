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
    console.log(body.errors[0].extensions);
    expect(body.data.transactions[0].amount).to.be.a("number");
  });
});
