import { CreateContributionInput } from "../../input-types/create-contribution.input-type";
import { IDonorInput } from "../../queries/search-donors.decoder";

export const createContributionInputToDonorInput = ({
  cardCVC,
  cardNumber,
  cardExpirationYear,
  cardExpirationMonth,
  amount,
  paymentDate,
  paymentMethod,
  committeeId,
  checkNumber,
  ...rest
}: CreateContributionInput): IDonorInput => rest;
