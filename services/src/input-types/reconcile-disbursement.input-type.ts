import { Field, InputType } from "type-graphql";

@InputType()
export class ReconcileDisbursementInput {
  @Field((type) => [String])
  selectedTransactions: string[];

  @Field()
  bankTransaction: string;

  @Field()
  committeeId: string;
}
