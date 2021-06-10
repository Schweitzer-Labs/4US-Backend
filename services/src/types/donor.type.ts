import { Field, ID, ObjectType, registerEnumType } from "type-graphql";
import { EntityType } from "../utils/enums/entity-type.enum";
import { EmploymentStatus } from "../utils/enums/employment-status";

registerEnumType(EntityType, {
  name: "EntityType",
});

registerEnumType(EmploymentStatus, {
  name: "EmploymentStatus",
});

@ObjectType()
export class Donor {
  @Field((type) => ID)
  id: string;

  @Field((type) => String)
  firstName: string;

  @Field((type) => String)
  middleName: string;

  @Field((type) => String)
  lastName: string;

  @Field((type) => String)
  addressLine1: string;

  @Field((type) => String)
  addressLine2: string;

  @Field((type) => String)
  city: string;

  @Field((type) => String)
  state: string;

  @Field((type) => String)
  postalCode: string;

  @Field((type) => EntityType)
  entityType: EntityType;

  @Field((type) => EmploymentStatus)
  employmentStatus: EmploymentStatus;
}
