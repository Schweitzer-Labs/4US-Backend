import { Field, ID, ObjectType } from "type-graphql";
import { IAggregations } from "../utils/model/txns-to-agg.utils";

@ObjectType()
export class Aggregations implements IAggregations {
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
