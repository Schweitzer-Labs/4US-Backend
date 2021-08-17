import { Field, InputType } from "type-graphql";
import { ITransaction } from "../queries/search-transactions.decoder";
import { MinLength } from "class-validator";

@InputType()
export class GenCommitteeInput {
  @Field()
  @MinLength(3)
  password: string;
}
