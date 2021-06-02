import { Field, InputType, registerEnumType } from "type-graphql";
import { ITransaction } from "../queries/search-transactions.decoder";

enum EntityType {
  CAN = "can",
  FAM = "fam",
  IND = "ind",
  SOLEP = "solep",
  PART = "part",
  CORP = "corp",
  COMM = "comm",
  UNION = "union",
  ASSOC = "assoc",
  LLC = "llc",
  PAC = "pac",
  PLC = "plc",
  OTH = "oth",
}

registerEnumType(EntityType, {
  name: "Entity Type", // this one is mandatory
  description: "Type of entity involved in the transaction", // this one is optional
});

@InputType()
export class CreateContributionInput implements Partial<ITransaction> {
  @Field()
  committeeId: string;

  @Field()
  amount: number;

  @Field()
  paymentMethod: string;

  @Field({ nullable: true })
  emailAddress?: string;

  @Field()
  firstName: string;

  @Field()
  lastName: string;

  @Field()
  addressLine1: string;

  @Field()
  city: string;

  @Field()
  state: string;

  @Field()
  postalCode: string;

  @Field()
  entityType: EntityType;

  @Field({ nullable: true })
  paymentDate?: number;

  // Required for PaymentMethod.Credit
  @Field({ nullable: true })
  cardNumber?: string;

  // Required for PaymentMethod.Credit
  @Field({ nullable: true })
  cardExpirationMonth?: number;

  // Required for PaymentMethod.Credit
  @Field({ nullable: true })
  cardExpirationYear?: number;

  // Required for PaymentMethod.Credit
  @Field({ nullable: true })
  cardCVC?: string;

  // Required for PaymentMethod.ACH and Check
  @Field({ nullable: true })
  checkNumber?: string;

  // Required if entityType is not ind or fam
  @Field({ nullable: true })
  entityName?: string;

  @Field({ nullable: true })
  employer?: string;

  @Field({ nullable: true })
  occupation?: string;

  @Field({ nullable: true })
  middleName?: string;

  @Field({ nullable: true })
  refCode?: string;

  @Field({ nullable: true })
  addressLine2?: string;

  @Field({ nullable: true })
  companyName?: string;

  @Field({ nullable: true })
  phoneNumber?: string;

  @Field({ nullable: true })
  attestsToBeingAnAdultCitizen?: boolean;
}
