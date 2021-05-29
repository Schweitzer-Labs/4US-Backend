import { Arg, Args, Mutation, Query, Resolver } from "type-graphql";

import { Committee } from "../types/committee.type";
import { Transaction } from "../types/transaction.type";
import { Aggregations } from "../types/aggregations.type";
import { Service } from "typedi";
import { DynamoDB } from "aws-sdk";
import * as AWS from "aws-sdk";
import * as dotenv from "dotenv";
import { searchTransactions } from "../queries/search-transactions.query";
import { TransactionsArg } from "../args/transactions.arg";
import { isLeft } from "fp-ts/Either";
import CurrentUser from "../decorators/current-user.decorator";
import { loadCommitteeOrThrow } from "../utils/model/load-committee-or-throw.utils";
import { CreateContributionInput } from "../input-types/create-contribution.input-type";
import { putTransaction } from "../utils/model/put-transaction.utils";
import { TransactionType } from "../utils/enums/transaction-type.enum";
import { ITransaction } from "../queries/search-transactions.decoder";
import { genTxnId } from "../utils/gen-txn-id.utils";
import { Direction } from "../utils/enums/direction.enum";
import { now } from "../utils/time.utils";
import { Source } from "../utils/enums/source.enum";
import { stripCardInfo } from "../utils/strip-card-info";

dotenv.config();

const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const donorsTableName: any = process.env.DONORS_DDB_TABLE_NAME;

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const dynamoDB = new DynamoDB();

@Service()
@Resolver()
export class AppResolver {
  @Query((returns) => Committee)
  async committee(
    @Arg("committeeId") committeeId: string,
    @CurrentUser() currentUser: string
  ) {
    return await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(
      committeeId
    )(currentUser);
  }

  @Query((returns) => [Transaction])
  async transactions(
    @Args() transactionArgs: TransactionsArg,
    @CurrentUser() currentUser: string
  ): Promise<Transaction[]> {
    console.log("the table name");
    console.log(txnsTableName);
    const committee = await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(
      transactionArgs.committeeId
    )(currentUser);

    const res = await searchTransactions(txnsTableName)(dynamoDB)(
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
    await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(committeeId)(
      currentUser
    );

    const args = new TransactionsArg();
    args.committeeId = committeeId;
    const res = await searchTransactions(txnsTableName)(dynamoDB)(args)();
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
      if (txn.transactionType === TransactionType.CONTRIBUTION) {
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
      if (txn.transactionType === TransactionType.DISBURSEMENT) {
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

  @Mutation((returns) => Transaction)
  async createContribution(
    @Arg("createContributionData")
    createContributionData: CreateContributionInput,
    @CurrentUser() currentUser: string
  ): Promise<Transaction> {
    const { committeeId } = createContributionData;
    const committee = await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(
      committeeId
    )(currentUser);

    const txn: ITransaction = {
      // required field
      id: genTxnId(),
      direction: Direction.IN,
      bankVerified: false,
      ruleVerified: false,
      initiatedTimestamp: now(),
      source: Source.DASHBOARD,
      ...createContributionData,
    };

    return await putTransaction(txnsTableName)(dynamoDB)(txn);
  }

  async createDisbursement() {}

  async verifyDisbursement() {}
}
