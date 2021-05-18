import { Arg, Args, FieldResolver, Query, Resolver, Root } from "type-graphql";

import { Committee } from "./types/committee.type";
import { Donor } from "./types/donor.type";
import { Transaction } from "./types/transaction.type";
import { Aggregations } from "./types/aggregations.type";
import { Service } from "typedi";
import { DynamoDB } from "aws-sdk";
import * as AWS from "aws-sdk";
import * as dotenv from "dotenv";
import { searchTransactions } from "./queries/search-transactions.query";
import { isLeft } from "fp-ts/Either";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";

dotenv.config();

const runenv: any = process.env.RUNENV;

@Service()
@Resolver()
export class AppResolver {
  private readonly dynamoDB: DynamoDB;
  constructor() {
    AWS.config.apiVersions = {
      dynamodb: "2012-08-10",
    };
    AWS.config.update({ region: "us-east-1" });
    this.dynamoDB = new DynamoDB();
  }

  @Query((returns) => Committee)
  async committee(@Arg("committeeId") id: string): Promise<Committee> {
    return null;
  }

  @Query((returns) => [Transaction])
  async transactions(
    @Arg("committeeId") committeeId: string
  ): Promise<Transaction[]> {
    const res = await pipe(
      searchTransactions(runenv)(this.dynamoDB)(committeeId),
      taskEither.fold(
        (err) => task.of([]),
        (succ) => task.of(succ)
      )
    )();

    console.log(res);

    return res;
  }

  @Query((returns) => [Donor])
  async donors(@Arg("committeeId") committeeId: string): Promise<Donor[]> {
    return [];
  }

  @Query((returns) => [Donor])
  async aggregations(@Arg("committeeId") id: string): Promise<Aggregations> {
    return null;
  }
}
