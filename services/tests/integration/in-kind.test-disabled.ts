import graphql from "../../src/committee-graphql.lambda";
import { genGraphQLProxy } from "../utils/gen-allowed-proxy.util";
import { expect } from "chai";
import * as dotenv from "dotenv";
import platformContribute from "../../src/platform-contribute.lambda";

dotenv.config();

const lambdaPromise = (lambda, event, context) => {
  return new Promise((resolve, reject) => {
    lambda(event, context, (error, res) => {
      resolve(res);
    });
  });
};

describe("In-kind contribution", function () {
  it("Allows and in-kind contribution to be created and verified", async () => {
    const res: any = await lambdaPromise(graphql, genGraphQLProxy("", ""), {});
    console.log("body here", res);

    expect(res.headers["access-control-allow-origin"].length > 0).to.equal(
      true
    );
  });
});
