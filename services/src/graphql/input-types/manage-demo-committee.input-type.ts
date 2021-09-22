import { Field, InputType, registerEnumType } from "type-graphql";
import { MinLength } from "class-validator";

@InputType()
export class ManageDemoCommitteeInput {
  @Field()
  @MinLength(3)
  password: string;

  @Field()
  @MinLength(3)
  committeeId: string;
}
