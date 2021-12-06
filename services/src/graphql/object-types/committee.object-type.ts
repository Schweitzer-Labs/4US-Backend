import { Field, ID, ObjectType } from "type-graphql";

@ObjectType()
export class Committee {
  @Field((type) => ID)
  id: string;

  @Field()
  committeeName: string;

  @Field()
  candidateFirstName: string;

  @Field({ nullable: true })
  candidateMiddleName?: string;

  @Field()
  candidateLastName: string;

  @Field({ nullable: true })
  bankName?: string;

  @Field({ nullable: true })
  state?: string;

  @Field({ nullable: true })
  scope?: string;

  @Field({ nullable: true })
  officeType?: string;

  @Field({ nullable: true })
  party?: string;

  @Field({ nullable: true })
  race?: string;

  @Field({ nullable: true })
  district?: string;

  @Field({ nullable: true })
  county?: string;

  @Field({ nullable: true })
  ruleVersion?: string;

  @Field({ nullable: true })
  tzDatabaseName?: string;

  @Field({ nullable: true })
  platformPlan?: string;

  @Field({ nullable: true })
  emailAddresses?: string;

  @Field({ nullable: true })
  efsFilerId?: number;

  @Field({ nullable: true })
  efsElectionId?: number;
}
