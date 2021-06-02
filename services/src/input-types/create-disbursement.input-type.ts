import { Field, InputType, registerEnumType } from "type-graphql";
import { ITransaction } from "../queries/search-transactions.decoder";
import { PaymentMethod } from "../utils/enums/payment-method.enum";
import { PurposeCode } from "../utils/enums/purpose-code.enum";

registerEnumType(PurposeCode, {
  name: "PurposeCode",
  description: "Purpose code for an expenditure",
});

registerEnumType(PaymentMethod, {
  name: "PaymentMethod",
  description: "Payment method used in an expenditure",
});

@InputType()
export class CreateDisbursementInput implements Partial<ITransaction> {
  @Field()
  committeeId: string;

  @Field()
  amount: number;

  @Field()
  isSubcontracted: boolean;

  @Field()
  isPartialPayment: boolean;

  @Field()
  isExistingLiability: boolean;

  @Field((type) => PaymentMethod)
  paymentMethod: PaymentMethod;

  @Field((type) => PurposeCode)
  purposeCode: PurposeCode;

  @Field({ nullable: true })
  entityName: string;

  @Field()
  addressLine1: string;

  @Field()
  city: string;

  @Field()
  state: string;

  @Field()
  postalCode: string;

  @Field()
  paymentDate: number;

  // Required for PaymentMethod.Check
  @Field({ nullable: true })
  checkNumber?: string;

  @Field({ nullable: true })
  addressLine2?: string;
}
