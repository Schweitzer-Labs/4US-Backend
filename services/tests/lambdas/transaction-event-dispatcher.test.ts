import { expect } from "chai";
import transactionEventDispatcher from "../../src/transaction-event-dispatcher.lambda";
import { insertContributionEvent } from "../events/insert-contribution-event.ddb";
import { seedCommittees } from "../seed/seed-committees.job";

describe("Transaction Event Dispatch", function () {
  it("Publishes a web contribution to a queue", async () => {
    const payload: any = insertContributionEvent;
    const res = await transactionEventDispatcher(payload, {});
    expect(res.status).to.equal("success");
    expect(res.effect).to.equal("sqs_receipt_message_sent");
  });
});
