import { ITransaction } from "../../../model/transaction.type";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../application-error";
import { pipe } from "fp-ts/function";
import { listIsNotEmpty } from "../../list-is-not-empty.utils";
