import { Field, InputType, registerEnumType } from "type-graphql";
import { MinLength } from "class-validator";
import { TransactionType } from "../../utils/enums/transaction-type.enum";
import { EntityType } from "../../utils/enums/entity-type.enum";
import { ExternalSource } from "../../utils/enums/source.enum";

registerEnumType(ExternalSource, {
  name: "ExternalSource",
  description: "The Source of the transaction if external",
});

@InputType()
export class SeedExtContribsInput {
  @Field()
  @MinLength(3)
  password: string;

  @Field()
  @MinLength(3)
  committeeId: string;

  @Field((type) => ExternalSource)
  externalSource: ExternalSource;
}
