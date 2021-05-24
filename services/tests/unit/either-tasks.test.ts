import { expect } from "chai";
import { eventToContribution } from "../../src/either-tasks/event-to-contribution";

import { invalidPolicapitalContributeProxy } from "../events/invalid-policapital-contribute.proxy";
import { validPolicapitalContributeProxy } from "../events/valid-policapital-contribute-proxy";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";
import { StatusCodes } from "http-status-codes";

describe("Tests event to contribution monad", function () {
  it("Stops a contribution call with an invalid payload", async () => {
    const res: any = await pipe(
      eventToContribution(invalidPolicapitalContributeProxy),
      taskEither.fold(
        (err) => task.of(err.statusCode),
        (succ) => task.of(StatusCodes.OK)
      )
    )();
    expect(res).to.equal(400);
  });
  it("Allows a contribution call with an valid payload", async () => {
    const res = await pipe(
      eventToContribution(validPolicapitalContributeProxy),
      taskEither.fold(
        (err) => task.of(err.message),
        (succ) => task.of(succ.cardCVC)
      )
    )();
    expect(res).to.equal("123");
  });
});
