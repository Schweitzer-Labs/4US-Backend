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
export class AmendDisbInput implements Partial<ITransaction> {
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

  @Field({ nullable: true })
  state?: string;

  @Field({ nullable: true })
  postalCode?: string;

  @Field({ nullable: true })
  paymentDate?: number;

  // Required for PaymentMethod.Check
  @Field({ nullable: true })
  checkNumber?: string;
}
