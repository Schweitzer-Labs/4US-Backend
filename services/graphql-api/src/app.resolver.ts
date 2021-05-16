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
  private readonly esClient: Client;
  constructor() {
    dotenv.config();
    const esHost =
      process.env.ES_HOST || "http://docker.for.mac.localhost:9200";

    this.esClient = new Client({
      node: [
        "https://search-dev-purplepay-ovi7jfjjjejvjfwxmxi4ecw3ea.us-east-1.es.amazonaws.com/",
      ],
    });
  }

  @Query((returns) => Committee)
  async committee(@Arg("committeeId") id: string): Promise<Committee> {
    return null;
  }

  @Query((returns) => [Transaction])
  async transactions(
    @Arg("committeeId") committeeId: string
  ): Promise<Transaction[]> {
    const res = await this.esClient.search({
      size: 5000,
      index: "transaction_complete",
      body: {
        query: {
          bool: {
            must: [{ match: { committeeId } }],
          },
        },
      },
    });
    return res.body.hits.hits.map(({ _source }) => _source);
  }

  @Query((returns) => [Donor])
  async donors(@Arg("committeeId") committeeId: string): Promise<Donor[]> {
    const res = await this.esClient.search({
      size: 5000,
      index: "donor",
      body: {
        query: {
          bool: {
            must: [{ match: { committeeId } }],
          },
        },
      },
    });

    return res.body.hits.hits.map(({ _source }) => _source);
  }

  @Query((returns) => [Donor])
  async aggregations(@Arg("committeeId") id: string): Promise<Aggregations> {
    return null;
  }
}
