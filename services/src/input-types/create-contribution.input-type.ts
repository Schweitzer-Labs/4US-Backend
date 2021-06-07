import { Field, InputType, registerEnumType } from "type-graphql";
import { ITransaction } from "../queries/search-transactions.decoder";
import { PaymentMethod } from "../utils/enums/payment-method.enum";
import { EntityType } from "../utils/enums/entity-type.enum";
import {
  MinLength,
  Min,
  IsEmail,
  IsNumber,
  Max,
  MaxLength,
} from "class-validator";

registerEnumType(EntityType, {
  name: "EntityType",
  description: "Type of entity involved in the transaction",
});

registerEnumType(PaymentMethod, {
  name: "PaymentMethod",
  description: "Payment method of a contribution",
});

@InputType()
export class CreateContributionInput implements Partial<ITransaction> {
  @Field()
  @MinLength(3)
  committeeId: string;

  @Field()
  @Min(50)
  amount: number;

  @Field((type) => PaymentMethod)
  paymentMethod: PaymentMethod;

  @Field()
  @MinLength(1)
  firstName: string;

  @Field()
  @MinLength(1)
  lastName: string;

  @Field()
  @MinLength(1)
  addressLine1: string;

  @Field()
  @MinLength(1)
  city: string;

  @Field()
  @MinLength(2)
  state: string;

  @Field()
  @MinLength(5)
  postalCode: string;

  @Field((type) => EntityType)
  entityType: EntityType;

  @Field({ nullable: true })
  @IsEmail()
  emailAddress?: string;

  @Field({ nullable: true })
  @IsNumber()
  @Min(1)
  paymentDate?: number;

  // Required for PaymentMethod.Credit
  @Field({ nullable: true })
  @MinLength(13)
  cardNumber?: string;

  // Required for PaymentMethod.Credit
  @Field({ nullable: true })
  @Min(1)
  @Max(12)
  cardExpirationMonth?: number;

  // Required for PaymentMethod.Credit
  @Field({ nullable: true })
  @Min(2020)
  @Max(2060)
  cardExpirationYear?: number;

  // Required for PaymentMethod.Credit
  @Field({ nullable: true })
  @MinLength(1)
  @MaxLength(5)
  cardCVC?: string;

  // Required for PaymentMethod.ACH and Check
  @Field({ nullable: true })
  @MinLength(1)
  checkNumber?: string;

  // Required if entityType is not ind or fam
  @Field({ nullable: true })
  @MinLength(1)
  entityName?: string;

  @Field({ nullable: true })
  @MinLength(1)
  employer?: string;

  @Field({ nullable: true })
  @MinLength(1)
  occupation?: string;

  @Field({ nullable: true })
  @MinLength(1)
  middleName?: string;

  @Field({ nullable: true })
  refCode?: string;

  @Field({ nullable: true })
  @MinLength(1)
  addressLine2?: string;

  @Field({ nullable: true })
  @MinLength(1)
  companyName?: string;

  @Field({ nullable: true })
  @MinLength(4)
  phoneNumber?: string;

  @Field({ nullable: true })
  attestsToBeingAnAdultCitizen?: boolean;
}
