import graphql from "../../src/graphql";
import { validGraphQLProxy } from "../events/valid-graphql-proxy";

describe("GraphQL Endpoint Lambda", function () {
  it("Accepts a valid graphQL query", async () => {
    const testEvent = validGraphQLProxy;
    const testContext: any = {};
    graphql(testEvent, testContext, async (res) => {
      console.log(res);
    });
  });
});
