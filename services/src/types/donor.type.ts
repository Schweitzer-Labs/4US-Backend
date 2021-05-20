import { Field, ID, ObjectType } from "type-graphql";

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

  @Field((type) => String)
  contributeType: string;
}
