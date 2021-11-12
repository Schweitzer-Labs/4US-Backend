import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "./application-error";
import { taskEither as te } from "fp-ts";

export const mLog =
  <a>(msg: string) =>
  (data: a): TaskEither<ApplicationError, a> => {
    console.log(msg);
    console.log(JSON.stringify(data));
    return te.right(data);
  };
