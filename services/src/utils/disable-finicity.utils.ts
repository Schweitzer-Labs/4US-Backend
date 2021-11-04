import { ICommittee } from "../model/committee.type";

export const disableFinicity = ({
  finicityCustomerId,
  finicityAccountId,
  ...rest
}: ICommittee) => rest;
