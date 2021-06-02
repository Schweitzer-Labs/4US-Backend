import { Field, InputType, registerEnumType } from "type-graphql";
import { ITransaction } from "../queries/search-transactions.decoder";
import { PaymentMethod } from "../utils/enums/payment-method.enum";
import { PurposeCode } from "../utils/enums/purpose-code.enum";

registerEnumType(PurposeCode, {
  name: "Purpose Code",
  description: "Purpose code for an expenditure",
});

registerEnumType(PaymentMethod, {
  name: "Payment Method",
  description: "Payment method used in an expenditure",
});

@InputType()
export class VerifyDisbursementInput implements Partial<ITransaction> {
  @Field()
  entityName?: string;

  @Field()
  isSubcontracted: boolean;

  @Field()
  isPartialPayment: boolean;

  @Field()
  isExistingLiability: boolean;

  @Field((type) => PaymentMethod)
  paymentMethod?: PaymentMethod;

  @Field((type) => PurposeCode)
  purposeCode: PurposeCode;

  @Field()
  addressLine1: string;

  @Field({ nullable: true })
  addressLine2?: string;

  @Field()
  city: string;

  @Field()
  state: string;

  @Field()
  postalCode: string;

  // Required for PaymentMethod.Check
  @Field({ nullable: true })
  checkNumber?: string;
}
