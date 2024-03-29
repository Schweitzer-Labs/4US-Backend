import { AmendDisbInput } from "../../src/graphql/input-types/amend-disb.input-type";
interface GenAmendDisbConfig {
  committeeId: string;
  transactionId: string;
  entityName: string;
}

export const genAmendDisbInput = ({
  committeeId,
  transactionId,
  entityName,
}: GenAmendDisbConfig): AmendDisbInput => ({
  committeeId,
  transactionId,
  entityName,
});
