import { Field, ID, ObjectType } from "type-graphql";

@ObjectType()
export class Aggregations {
  @Field()
  balance: number;

  @Field()
  totalRaised: number;

  @Field()
  totalSpent: number;

  @Field()
  totalDonors: number;

  @Field()
  totalTransactions: number;

  @Field()
  totalContributionsInProcessing: number;

  @Field()
  totalDisbursementsInProcessing: number;

  @Field()
  needsReviewCount: number;
}
