import { Field, InputType, registerEnumType } from "type-graphql";
import { MinLength } from "class-validator";
import { TransactionType } from "../../utils/enums/transaction-type.enum";

registerEnumType(TransactionType, {
  name: "TransactionType",
});

@InputType()
export class SeedDemoBankRecordsInput {
  @Field()
  @MinLength(3)
  password: string;

  @Field()
  @MinLength(3)
  committeeId: string;

  @Field((type) => TransactionType)
  transactionType?: TransactionType;

  @Field({ nullable: true })
  amount: number;
}
