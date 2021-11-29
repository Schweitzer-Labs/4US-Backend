import { Field, InputType, registerEnumType } from "type-graphql";
import { Min, MinLength } from "class-validator";
import { PurposeCode } from "../../utils/enums/purpose-code.enum";
import { PaymentMethod } from "../../utils/enums/payment-method.enum";
import { State } from "../../utils/enums/state.enum";
import { ITransaction } from "../../queries/search-transactions.decoder";

registerEnumType(PurposeCode, {
  name: "PurposeCode",
  description: "Purpose code for an expenditure",
});

registerEnumType(PaymentMethod, {
  name: "PaymentMethod",
  description: "Payment method used in an expenditure",
});

registerEnumType(State, {
  name: "State",
  description: "State location of donor",
});

@InputType()
export class CreateDisbursementInput {
  @Field()
  @MinLength(3)
  committeeId: string;

  @Field()
  @Min(1)
  amount: number;

  @Field((type) => PaymentMethod)
  paymentMethod: PaymentMethod;

  @Field()
  @MinLength(1)
  entityName: string;

  @Field()
  @MinLength(1)
  addressLine1: string;

  @Field()
  @MinLength(1)
  city: string;

  @Field((type) => State)
  state: State;

  @Field()
  @MinLength(5)
  postalCode: string;

  @Field()
  isSubcontracted: boolean;

  @Field()
  isPartialPayment: boolean;

  @Field()
  isExistingLiability: boolean;

  @Field((type) => PurposeCode)
  purposeCode: PurposeCode;

  @Field()
  @Min(1)
  paymentDate: number;

  // Required for PaymentMethod.Check
  @Field({ nullable: true })
  checkNumber?: string;

  @Field({ nullable: true })
  @MinLength(1)
  addressLine2?: string;

  @Field({ nullable: true })
  explanation?: string;
}
