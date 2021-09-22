import { Field, InputType, registerEnumType } from "type-graphql";

import { IsEmail, IsNumber, Min, MinLength } from "class-validator";
import { EntityType } from "../../utils/enums/entity-type.enum";
import { EmploymentStatus } from "../../utils/enums/employment-status";
import { State } from "../../utils/enums/state.enum";
import {
  InKindType,
  PaymentMethod,
} from "../../utils/enums/payment-method.enum";
import { ITransaction } from "../../queries/search-transactions.decoder";

registerEnumType(EntityType, {
  name: "EntityType",
  description: "Type of entity involved in the transaction",
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
export class AmendContributionInput implements Partial<ITransaction> {
  @Field()
  transactionId?: string;

  @Field()
  @MinLength(3)
  committeeId: string;

  @Field({ nullable: true })
  @Min(50)
  amount?: number;

  @Field((type) => PaymentMethod, { nullable: true })
  paymentMethod?: PaymentMethod;

  @Field({ nullable: true })
  @MinLength(1)
  firstName?: string;

  @Field({ nullable: true })
  @MinLength(1)
  lastName?: string;

  @Field({ nullable: true })
  @MinLength(1)
  addressLine1?: string;

  @Field({ nullable: true })
  @MinLength(1)
  city?: string;

  @Field((type) => State, { nullable: true })
  state?: State;

  @Field({ nullable: true })
  @MinLength(5)
  postalCode?: string;

  @Field((type) => EntityType, { nullable: true })
  entityType?: EntityType;

  @Field({ nullable: true })
  @IsEmail()
  emailAddress?: string;

  @Field({ nullable: true })
  @IsNumber()
  @Min(1)
  paymentDate?: number;

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
  @MinLength(1)
  inKindDescription?: string;
}
