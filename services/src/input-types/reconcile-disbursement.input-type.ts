import { Field, InputType } from "type-graphql";

@InputType()
export class ReconcileDisbursementInput {
  @Field()
  selectedTransactions: string[];

  @Field()
  bankTransaction: string;
}
