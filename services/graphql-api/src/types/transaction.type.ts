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
  @Field()
  bankVerifiedTimestamp?: string;
  @Field()
  ruleVerifiedTimestamp?: string;
  @Field()
  purposeCode?: string;
  @Field()
  refCode?: string;
  // Donor Props
  @Field()
  firstName?: string;
  @Field()
  middleName?: string;
  @Field()
  lastName?: string;
  @Field()
  addressLine1: string;
  @Field()
  addressLine2?: string;
  @Field()
  city?: string;
  @Field()
  state?: string;
  @Field()
  postalCode?: string;
  @Field()
  employer?: string;
  @Field()
  occupation?: string;
  @Field()
  entityType?: string;
  @Field()
  companyName?: string;
  @Field()
  phoneNumber?: string;
  @Field()
  emailAddress?: string;
  @Field()
  attestsToBeingAnAdultCitizen?: boolean;
  @Field()
  transactionType?: string;
}
