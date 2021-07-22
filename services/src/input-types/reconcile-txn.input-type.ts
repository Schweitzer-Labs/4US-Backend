import { Field, InputType } from "type-graphql";

@InputType()
export class ReconcileTxnInput {
  @Field((type) => [String])
  selectedTransactions: string[];

  @Field()
  bankTransaction: string;

  @Field()
  committeeId: string;
}
