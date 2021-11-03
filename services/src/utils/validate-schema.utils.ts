// @ToDo generalize function
import {left, right, TaskEither} from "fp-ts/TaskEither";
import {ApplicationError} from "./application-error";
import {StatusCodes} from "http-status-codes";
import {ObjectSchema} from "joi";

export const validateObject = (schema: ObjectSchema) => (
    event: any
): TaskEither<ApplicationError, object> => {
    const res = schema.validate(event);
    if (res.error) {
        return left(
            new ApplicationError(res.error.message, {}, StatusCodes.BAD_REQUEST)
        );
    } else {
        return right(res.value);
    }
};
