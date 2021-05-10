import { expect } from "chai";
import { eventToContribution } from "../../src/either-tasks/event-to-contribution";

import invalidContributeEvent from "../events/invalid-contribute";
import validContributeEvent from "../events/valid-contribute";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";

describe("Tests event to contribution monad", function () {
  it("Stops a contribution call with an invalid payload", async () => {
    const res: number = await pipe(
      eventToContribution(invalidContributeEvent),
      taskEither.fold(
        (err) => task.of(err.statusCode),
        (succ) => task.of(200)
      )
    )();
    expect(res).to.equal(400);
  });
  it("Allows a contribution call with an valid payload", async () => {
    const res = await pipe(
      eventToContribution(validContributeEvent),
      taskEither.fold(
        (err) => task.of(err.message),
        (succ) => task.of(succ.cardCVC)
      )
    )();
    expect(res).to.equal("123");
  });
});
