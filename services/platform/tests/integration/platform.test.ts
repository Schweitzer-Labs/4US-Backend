import { expect } from "chai";
import { pipe } from "fp-ts/function";
import * as Platform from "../../src/clients/platform/platform.client";
import { task, taskEither } from "fp-ts";
import * as connectionClass from "http-aws-es";
import { Client } from "@elastic/elasticsearch";

let esClient: Client;
let committeeId: string;

describe("Tests platform transaction queries", function () {
  before(() => {
    esClient = new Client({
      node: ["http://localhost:9200"],
    });
    committeeId = "0aab3c20-a05a-465a-a3ae-93df7bb1dc88";
  });
  it("Pulls all transactions by committee ID", async () => {
    const transactions = await pipe(
      Platform.getUnverifiedTransactions(esClient)(committeeId),
      taskEither.fold(
        () => task.of([]),
        (transactions) => task.of(transactions)
      )
    )();

    expect(transactions.length > 0).to.equal(true);
  });
});
