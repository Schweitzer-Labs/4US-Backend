import { AmendContributionInput } from "../../src/input-types/amend-contrib.input-type";

interface GenAmendDisbConfig {
  committeeId: string;
  transactionId: string;
  addressLine1: string;
}

export const genAmendContribInput = ({
  committeeId,
  transactionId,
  addressLine1,
}: GenAmendDisbConfig): AmendContributionInput => ({
  committeeId,
  transactionId,
  addressLine1,
});
