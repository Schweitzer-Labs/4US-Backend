import { ICommittee } from "../types/committee.type";

export const disableFinicity = ({
  finicityCustomerId,
  finicityAccountId,
  ...rest
}: ICommittee) => rest;
