import { EntityType } from "../enums/entity-type.enum";

export interface IFlacspeeInput {
  entityName?: string;
  firstName: string;
  lastName: string;
  addressLine1: string;
  city: string;
  state: string;
  postalCode: string;
  entityType: EntityType;
  emailAddress?: string;
}

export const genFlacspee = ({
  firstName,
  lastName,
  addressLine1,
  city,
  state,
  postalCode,
  entityType,
  emailAddress,
  entityName,
}: IFlacspeeInput) => {
  // Put entity name in last name field and leave first name blank when donor is a non-individual
  const fl =
    entityType === EntityType.IND
      ? `[${firstName.toLowerCase()}][${lastName.toLowerCase()}]`
      : `[][${entityName.toLowerCase()}]`;
  const acspe = `[${addressLine1.toLowerCase()}][${city.toLowerCase()}][${state.toLowerCase()}][${postalCode.toLowerCase()}][${entityType.toLowerCase()}]`;
  // Exclude email when donor is a non-individual
  const e =
    entityType === EntityType.IND
      ? `[${(emailAddress || "").toLowerCase()}]`
      : `[]`;
  return `flacspee:${fl}${acspe}${e}`;
};
