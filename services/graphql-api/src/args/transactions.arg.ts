import { ArgsType, Field, Int } from "type-graphql";
import { Max, Min } from "class-validator";

@ArgsType()
class TransactionsArg {
  @Field((type) => Int)
  @Min(0)
  skip: number = 0;

  @Field((type) => Int)
  @Min(1)
  @Max(50)
  take: number = 25;
}
