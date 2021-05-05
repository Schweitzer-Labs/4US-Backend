import { expect } from "chai";
import { eventToContribution } from "../../src/either-tasks/event-to-contribution";

import invalidContributeEvent from "../events/invalid-contribute";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";

describe("Tests event to contribution", function () {
  it("Stops a contribution call with an invalid payload", async () => {
    const res = pipe(taskEither.chain());
  });
});
