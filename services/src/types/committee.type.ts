import { Field, ID, ObjectType } from "type-graphql";

@ObjectType()
export class Committee {
  @Field((type) => ID)
  id: string;

  @Field((type) => String)
  committeeName: string;

  @Field((type) => String)
  candidateFirstName: string;

  @Field((type) => String)
  candidateMiddleName?: string;

  @Field((type) => String)
  candidateLastName: string;

  @Field((type) => String)
  bankName?: string;

  @Field((type) => String)
  state?: string;

  @Field((type) => String)
  scope?: string;

  @Field((type) => String)
  officeType?: string;

  @Field((type) => String)
  party?: string;

  @Field((type) => String)
  race?: string;

  @Field((type) => String)
  district?: string;

  @Field((type) => String)
  county?: string;

  @Field((type) => String)
  ruleVersion?: string;

  @Field((type) => String)
  tzDatabaseName?: string;

  @Field((type) => String)
  platformPlan?: string;

  @Field((type) => String)
  emailAddresses?: string;
}
