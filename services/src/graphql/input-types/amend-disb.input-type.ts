import { Field, InputType, registerEnumType } from "type-graphql";
import { PurposeCode } from "../../utils/enums/purpose-code.enum";
import { PaymentMethod } from "../../utils/enums/payment-method.enum";
import { State } from "../../utils/enums/state.enum";

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
export class AmendDisbInput {
  @Field()
  transactionId?: string;

  @Field()
  committeeId?: string;

  @Field({ nullable: true })
  entityName?: string;

  @Field({ nullable: true })
  isSubcontracted?: boolean;

  @Field({ nullable: true })
  isPartialPayment?: boolean;

  @Field({ nullable: true })
  isExistingLiability?: boolean;

  @Field((type) => PaymentMethod, { nullable: true })
  paymentMethod?: PaymentMethod;

  @Field((type) => PurposeCode, { nullable: true })
  purposeCode?: PurposeCode;

  @Field({ nullable: true })
  addressLine1?: string;

  @Field({ nullable: true })
  addressLine2?: string;

  @Field({ nullable: true })
  city?: string;

  @Field((type) => State, { nullable: true })
  state?: State;

  @Field({ nullable: true })
  postalCode?: string;

  @Field({ nullable: true })
  paymentDate?: number;

  // Required for PaymentMethod.Check
  @Field({ nullable: true })
  checkNumber?: string;
}
