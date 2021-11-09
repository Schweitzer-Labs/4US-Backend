import { Field, InputType, registerEnumType } from "type-graphql";
import {
  MinLength,
  Min,
  IsEmail,
  IsNumber,
  Max,
  MaxLength,
} from "class-validator";
import { Owner } from "./owner.input-type";
import { EmploymentStatus } from "../../utils/enums/employment-status";
import { State } from "../../utils/enums/state.enum";
import { EntityType } from "../../utils/enums/entity-type.enum";
import {
  InKindType,
  PaymentMethod,
} from "../../utils/enums/payment-method.enum";
import { ITransaction } from "../../model/transaction.type";

registerEnumType(EntityType, {
  name: "EntityType",
  description: "Type of entity involved in the transaction",
});

registerEnumType(PaymentMethod, {
  name: "PaymentMethod",
  description: "Payment method of a contribution",
});

registerEnumType(EmploymentStatus, {
  name: "EmploymentStatus",
  description: "Employment status of donor",
});

registerEnumType(State, {
  name: "State",
  description: "State location of donor",
});

registerEnumType(InKindType, {
  name: "InKindType",
  description: "Type of In-kind contribution",
});

@InputType()
export class CreateContributionInput {
  @Field()
  @MinLength(3)
  committeeId: string;

  @Field()
  @Min(50)
  amount: number;

  @Field()
  processPayment: boolean;

  @Field((type) => PaymentMethod)
  paymentMethod: PaymentMethod;

  @Field((type) => [Owner], { nullable: true })
  owners?: Owner[];

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

  @Field((type) => State)
  state: State;

  @Field()
  @MinLength(5)
  postalCode: string;

  @Field((type) => EntityType)
  entityType: EntityType;

  @Field({ nullable: true })
  @IsEmail()
  emailAddress?: string;

  @Field()
  @IsNumber()
  @Min(1)
  paymentDate: number;

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

  @Field((type) => EmploymentStatus, { nullable: true })
  employmentStatus?: EmploymentStatus;

  @Field((type) => InKindType, { nullable: true })
  inKindType?: InKindType;

  @Field({ nullable: true })
  @MinLength(2)
  inKindDescription?: string;

  actBlueTransactionId?: string;
}
