import { IDonorInput } from "../../queries/search-donors.decoder";
import { CreateContributionInput } from "../../graphql/input-types/create-contribution.input-type";
import { ContribInput } from "../../graphql/input-types/contrib-input.input-type";

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