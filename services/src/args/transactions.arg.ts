import { ArgsType, Field, Int } from "type-graphql";
import { Max, Min } from "class-validator";
import { Order } from "../utils/enums/order.enum";
import { registerEnumType } from "type-graphql";

registerEnumType(Order, {
  name: "Order", // this one is mandatory
  description: "Order by initiated timestamp", // this one is optional
});

@ArgsType()
export class TransactionsArg {
  @Field((type) => String)
  committeeId: string;

  @Field((type) => Order, { nullable: true })
  order?: Order;

  @Field((type) => String, { nullable: true })
  transactionType?: string;

  @Field((type) => Boolean, { nullable: true })
  ruleVerified?: boolean;

  @Field((type) => Boolean, { nullable: true })
  bankVerified?: boolean;

  @Field((type) => String, { nullable: true })
  donorId?: string;
}
