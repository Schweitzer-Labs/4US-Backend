import { Field, InputType, registerEnumType } from "type-graphql";
import { MinLength } from "class-validator";
import { DemoType } from "../../utils/enums/demo-type.enum";

registerEnumType(DemoType, {
  name: "DemoType",
  description: "Type of transaction set for demoing",
});

@InputType()
export class GenCommitteeInput {
  @Field()
  @MinLength(3)
  password: string;

  @Field((type) => DemoType, { nullable: true })
  demoType?: DemoType;
}
