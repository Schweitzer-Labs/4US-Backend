import { PaymentMethod } from "../../utils/enums/payment-method.enum";
import { ValidationError } from "apollo-server-lambda";
import { CreateContributionInput } from "../input-types/create-contribution.input-type";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";
import { taskEither } from "fp-ts";

export const validateCard = (
  contribInput: CreateContributionInput
): TaskEither<ApplicationError, boolean> => {
  console.log("validate card fired");
  const {
    paymentMethod,
    cardCVC,
    cardNumber,
    cardExpirationMonth,
    cardExpirationYear,
    processPayment,
  } = contribInput;

  if (
    [PaymentMethod.Credit, PaymentMethod.Debit].includes(paymentMethod) &&
    processPayment
  ) {
    if (
      !cardCVC ||
      !cardNumber ||
      !cardExpirationMonth ||
      !cardExpirationYear
    ) {
      console.log("card error caught");
      return taskEither.left(
        new ApplicationError(
          "Card info must be provided for contributions in credit and debit",
          contribInput
        )
      );
    }
  }
  return taskEither.right(true);
};
