interface GenAmendDisbConfig {
  committeeId: string;
  transactionId: string;
  entityName: string;
}

export const genAmendDisbInput = ({
  committeeId,
  transactionId,
  entityName,
}: GenAmendDisbConfig) => ({
  committeeId,
  transactionId,
  entityName,
});
