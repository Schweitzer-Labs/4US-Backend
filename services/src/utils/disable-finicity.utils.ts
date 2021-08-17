import { ICommittee } from "../queries/get-committee-by-id.query";

export const disableFinicity = ({
  finicityCustomerId,
  finicityAccountId,
  ...rest
}: ICommittee) => rest;
