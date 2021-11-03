import { PaymentMethod } from "../../utils/enums/payment-method.enum";
import { ValidationError } from "apollo-server-lambda";
import { CreateContributionInput } from "../input-types/create-contribution.input-type";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";
import { taskEither } from "fp-ts";
import { CreateDisbursementInput } from "../input-types/create-disbursement.input-type";

export const validateCheck = (
  d: CreateDisbursementInput
): TaskEither<ApplicationError, boolean> => {
  if (d.paymentMethod === PaymentMethod.Check && !d.checkNumber)
    return taskEither.left(
      new ApplicationError("Check number must be provided.", d)
    );

  return taskEither.right(true);
};
