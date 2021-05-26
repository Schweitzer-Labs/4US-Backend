import {
  Arg,
  Args,
  Ctx,
  FieldResolver,
  Query,
  Resolver,
  Root,
  UnauthorizedError,
} from "type-graphql";

import { Committee } from "../types/committee.type";
import { Donor } from "../types/donor.type";
import { Transaction } from "../types/transaction.type";
import { Aggregations } from "../types/aggregations.type";
import { Service } from "typedi";
import { DynamoDB } from "aws-sdk";
import * as AWS from "aws-sdk";
import * as dotenv from "dotenv";
import { searchTransactions } from "../queries/search-transactions.query";
import { TransactionsArg } from "../args/transactions.arg";
import { getCommitteeById } from "../queries/get-committee-by-id.query";
import { isLeft } from "fp-ts/Either";
import CurrentUser from "../decorators/current-user.decorator";
import { loadCommitteeOrThrow } from "../utils/model/load-committee-or-throw.utils";

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
    this.dynamoDB = new DynamoDB();
  }

  @Query((returns) => Committee)
  async committee(
    @Arg("committeeId") committeeId: string,
    @CurrentUser() currentUser: string
  ) {
    return await loadCommitteeOrThrow(`committees-${runenv}`)(this.dynamoDB)(
      committeeId
    )(currentUser);
  }

  @Query((returns) => [Transaction])
  async transactions(
    @Args() transactionArgs: TransactionsArg,
    @CurrentUser() currentUser: string
  ): Promise<Transaction[]> {
    const committee = await loadCommitteeOrThrow(`committees-${runenv}`)(
      this.dynamoDB
    )(transactionArgs.committeeId)(currentUser);

    const res = await searchTransactions(runenv)(this.dynamoDB)(
      transactionArgs
    )();
    if (isLeft(res)) {
      throw res.left;
    } else {
      return res.right;
    }
  }

  @Query((returns) => Aggregations)
  async aggregations(
    @Arg("committeeId") committeeId: string,
    @CurrentUser() currentUser: string
  ): Promise<Aggregations> {
    const committee = await loadCommitteeOrThrow(`committees-${runenv}`)(
      this.dynamoDB
    )(committeeId)(currentUser);

    const args = new TransactionsArg();
    args.committeeId = committeeId;
    const res = await searchTransactions(runenv)(this.dynamoDB)(args)();
    if (isLeft(res)) {
      throw res.left;
    }

    const transactions = res.right;

    const init: any = {
      balance: 0,
      totalRaised: 0,
      totalSpent: 0,
      totalDonors: 112,
      totalTransactions: transactions.length,
      totalContributionsInProcessing: 0,
      totalDisbursementsInProcessing: 0,
      needsReviewCount: 0,
      donorMap: {},
    };

    const aggs: any = transactions.reduce((acc: any, txn) => {
      // Total Raised
      if (txn.transactionType === "contribution") {
        if (txn.bankVerified) {
          acc.totalRaised = acc.totalRaised + txn.amount;
          acc.balance = acc.balance + txn.amount;
        } else {
          acc.totalContributionsInProcessing =
            acc.totalContributionsInProcessing + txn.amount;
        }
        const donorHash = `${txn.firstName}${txn.lastName}${txn.entityType}`;
        acc.donorMap[donorHash] = true;
      }
      /// Total Spent
      if (txn.transactionType === "disbursement") {
        if (txn.bankVerified) {
          acc.totalSpent = acc.totalSpent + txn.amount;
        } else {
          acc.totalDisbursementsInProcessing =
            acc.totalDisbursementsInProcessing + txn.amount;
          acc.balance = acc.balance - txn.amount;
        }
      }
      if (!txn.ruleVerified) {
        acc.needsReviewCount++;
      }

      return acc;
    }, init);

    aggs.totalDonors = Object.keys(aggs.donorMap).length;
    return aggs;
  }
}
