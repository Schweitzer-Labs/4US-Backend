import { Field, InputType, registerEnumType } from "type-graphql";
import { MinLength } from "class-validator";
import { State } from "../utils/enums/state.enum";

registerEnumType(State, {
  name: "State",
  description: "State location of donor",
});

@InputType()
export class Owner {
  @Field()
  @MinLength(1)
  firstName: string;

  @Field()
  @MinLength(1)
  lastName: string;

  @Field()
  @MinLength(1)
  addressLine1: string;

  @Field({ nullable: true })
  @MinLength(1)
  addressLine2: string;

  @Field()
  @MinLength(1)
  city: string;

  @Field((type) => State)
  state: State;

  @Field()
  @MinLength(5)
  postalCode: string;

  @Field()
  @MinLength(5)
  percentOwnership: number;
}
