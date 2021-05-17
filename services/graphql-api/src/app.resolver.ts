import { Arg, Args, FieldResolver, Query, Resolver, Root } from "type-graphql";

import { Committee } from "./types/committee.type";
import { Donor } from "./types/donor.type";
import { Transaction } from "./types/transaction.type";
import { Aggregations } from "./types/aggregations.type";
import { Service } from "typedi";
import { Client } from "@elastic/elasticsearch";
import * as dotenv from "dotenv";

@Service()
@Resolver()
export class AppResolver {
  constructor() {}

  @Query((returns) => Committee)
  async committee(@Arg("committeeId") id: string): Promise<Committee> {
    return null;
  }

  @Query((returns) => [Transaction])
  async transactions(
    @Arg("committeeId") committeeId: string
  ): Promise<Transaction[]> {
    return [];
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
