import { ArgsType, Field, Int } from "type-graphql";

@ArgsType()
export class TransactionArg {
  @Field((type) => String)
  committeeId: string;

  @Field((type) => String)
  id: string;
}
