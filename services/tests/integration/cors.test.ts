import { lambdaPromise } from "../../src/utils/lambda-promise.util";
import graphql from "../../src/committee-graphql.lambda";
import { genGraphQLProxy } from "../utils/gen-allowed-proxy.util";
import { expect } from "chai";
import * as dotenv from "dotenv";
import platformContribute from "../../src/platform-contribute.lambda";

dotenv.config();

describe("Cors", function () {
  it("GraphQL returns CORS Origin", async () => {
    const res: any = await lambdaPromise(graphql, genGraphQLProxy("", ""), {});
    console.log("body here", res);

    expect(res.headers["access-control-allow-origin"].length > 0).to.equal(
      true
    );
  });
  it("Platform Contribute returns CORS Origin", async () => {
    const res: any = await platformContribute({});
    console.log("body here", res);

    expect(res.headers["access-control-allow-origin"].length > 0).to.equal(
      true
    );
  });
});
