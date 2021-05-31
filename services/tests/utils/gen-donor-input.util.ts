import * as faker from "faker";
import { EntityType } from "../../src/utils/enums/entity-type.enum";
import { IDonorInput } from "../../src/queries/search-donors.decoder";

export const genDonorInput = (entityType: EntityType): IDonorInput => ({
  firstName: faker.name.firstName(),
  lastName: faker.name.lastName(),
  addressLine1: faker.address.streetAddress(),
  city: faker.address.city(),
  state: faker.address.stateAbbr().toLowerCase(),
  postalCode: faker.address.zipCode(),
  entityType,
});
