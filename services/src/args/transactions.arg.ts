import { ArgsType, Field, Int } from "type-graphql";
import { Max, Min } from "class-validator";

@ArgsType()
export class TransactionsArg {
  @Field((type) => String)
  committeeId: string;

  @Field((type) => String, { nullable: true })
  transactionType?: string;

  @Field((type) => Boolean, { nullable: true })
  ruleVerified?: boolean;

  @Field((type) => Boolean, { nullable: true })
  bankVerified?: boolean;
}
