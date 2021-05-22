import { Field, ID, ObjectType } from "type-graphql";

import { registerEnumType } from "type-graphql";
import { ITransaction } from "../queries/search-transactions.decoder";

enum Direction {
  IN = "in",
  OUT = "out",
}

registerEnumType(Direction, {
  name: "Direction", // this one is mandatory
  description: "Direction of payment", // this one is optional
});

@ObjectType()
export class Transaction implements ITransaction {
  @Field((type) => ID)
  id;
  @Field()
  committeeId: string;
  @Field()
  direction: string;
  @Field()
  amount: number;
  @Field()
  paymentMethod: string;
  @Field()
  bankVerified: boolean;
  @Field()
  ruleVerified: boolean;
  @Field()
  initiatedTimestamp: string;
  @Field({ nullable: true })
  bankVerifiedTimestamp?: string;
  @Field({ nullable: true })
  ruleVerifiedTimestamp?: string;
  @Field({ nullable: true })
  purposeCode?: string;
  @Field({ nullable: true })
  refCode?: string;
  // Donor Props
  @Field({ nullable: true })
  firstName?: string;
  @Field({ nullable: true })
  middleName?: string;
  @Field({ nullable: true })
  lastName?: string;
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
  employer?: string;
  @Field({ nullable: true })
  occupation?: string;
  @Field({ nullable: true })
  entityType?: string;
  @Field({ nullable: true })
  companyName?: string;
  @Field({ nullable: true })
  phoneNumber?: string;
  @Field({ nullable: true })
  emailAddress?: string;
  @Field({ nullable: true })
  attestsToBeingAnAdultCitizen?: boolean;
  @Field({ nullable: true })
  transactionType?: string;
  @Field({ nullable: true })
  stripePaymentIntentId?: string;
  @Field({ nullable: true })
  cardNumberLastFourDigits?: string;
}
