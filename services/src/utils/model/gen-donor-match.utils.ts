import { EntityType } from "../enums/entity-type.enum";

export interface IFlacspeeInput {
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
}: IFlacspeeInput) => {
  return `flacspee:[${firstName.toLowerCase()}][${lastName.toLowerCase()}][${addressLine1.toLowerCase()}][${city.toLowerCase()}][${state.toLowerCase()}][${postalCode.toLowerCase()}][${entityType.toLowerCase()}][${emailAddress.toLowerCase()}]`;
};
