import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";
import { taskEither } from "fp-ts";

export const validateDemoCommittee = (
  committeeId: string
): TaskEither<ApplicationError, string> =>
  committeeId.includes("josh-allen")
    ? taskEither.right(committeeId)
    : taskEither.left(new ApplicationError("Committee is not in demo", {}));
