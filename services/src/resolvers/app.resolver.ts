import { Arg, Args, Mutation, Query, Resolver } from "type-graphql";

import { Committee } from "../types/committee.type";
import { Transaction } from "../types/transaction.type";
import { Aggregations } from "../types/aggregations.type";
import { Service } from "typedi";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import * as dotenv from "dotenv";
import { searchTransactions } from "../queries/search-transactions.query";
import { TransactionsArg } from "../args/transactions.arg";
import { isLeft } from "fp-ts/Either";
import CurrentUser from "../decorators/current-user.decorator";
import { loadCommitteeOrThrow } from "../utils/model/load-committee-or-throw.utils";
import { CreateContributionInput } from "../input-types/create-contribution.input-type";
import { getLNPassword, getLNUsername, getStripeApiKey } from "../utils/config";
import { Stripe } from "stripe";
import { ValidationError } from "apollo-server-lambda";
import { PaymentMethod } from "../utils/enums/payment-method.enum";
import { CreateDisbursementInput } from "../input-types/create-disbursement.input-type";
import { createDisbInputToTxn } from "../utils/model/create-disbursement-input-to-transaction.utils";
import {
  putTransaction,
  putTransactionAndDecode,
} from "../utils/model/put-transaction.utils";
import { runRulesAndProcess } from "../pipes/run-rules-and-process.pipe";
import * as https from "https";
import { txnsToAgg } from "../utils/model/txns-to-agg.utils";
import { TransactionArg } from "../args/transaction.arg";
import { getTxnById } from "../utils/model/get-txn-by-id.utils";
import { ReconcileTxnInput } from "../input-types/reconcile-txn.input-type";
import { AmendDisbInput } from "../input-types/amend-disb.input-type";
import { amendDisb } from "../pipes/amend-disb.pipe";
import { AmendContributionInput } from "../input-types/amend-contrib.input-type";
import { amendContrib } from "../pipes/amend-contrib.pipe";
import { validateContribOrThrow } from "../utils/validate-contrib-or-throw.util";
import { reconcileTxnWithTxns } from "../pipes/reconcile-txn.pipe";
import { ILexisNexisConfig } from "../clients/lexis-nexis/lexis-nexis.client";
import { verifyAndCreateDisb } from "../pipes/verify-and-create-disb.pipe";

dotenv.config();

const billableEventsTableName: any = process.env.BILLABLE_EVENTS_DDB_TABLE_NAME;
const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const donorsTableName: any = process.env.DONORS_DDB_TABLE_NAME;
const rulesTableName: any = process.env.RULES_DDB_TABLE_NAME;
const runenv: any = process.env.RUNENV;

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

const agent = new https.Agent({
  keepAlive: true,
});

const dynamoDB = new DynamoDB({
  httpOptions: {
    agent,
  },
});

@Service()
@Resolver()
export class AppResolver {
  private stripeApiKey: string;
  private stripe: Stripe;
  private lnUsername: string;
  private lnPassword: string;
  @Query((returns) => Committee)
  async committee(
    @Arg("committeeId") committeeId: string,
    @CurrentUser() currentUser: string
  ) {
    console.log("from committee resolver");
    const res = await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(
      committeeId
    )(currentUser);

    console.log("committee response", res);

    return res;
  }

  @Query((returns) => [Transaction])
  async transactions(
    @Args() transactionsArg: TransactionsArg,
    @CurrentUser() currentUser: string
  ): Promise<Transaction[]> {
    await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(
      transactionsArg.committeeId
    )(currentUser);

    const res = await searchTransactions(txnsTableName)(dynamoDB)(
      transactionsArg
    )();
    if (isLeft(res)) {
      throw res.left;
    } else {
      return res.right;
    }
  }

  @Query((returns) => Transaction, { nullable: true })
  async transaction(
    @Args() transactionArg: TransactionArg,
    @CurrentUser() currentUser: string
  ): Promise<Transaction> {
    await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(
      transactionArg.committeeId
    )(currentUser);

    const res = await getTxnById(txnsTableName)(dynamoDB)(
      transactionArg.committeeId
    )(transactionArg.id)();
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

    const txns = res.right;
    return txnsToAgg(txns);
  }

  @Mutation((returns) => Transaction)
  async createContribution(
    @Arg("createContributionData")
    createContributionInput: CreateContributionInput,
    @CurrentUser() currentUser: string
  ): Promise<Transaction> {
    if (
      !this.stripeApiKey ||
      !this.stripe ||
      !this.lnUsername ||
      !this.lnPassword
    ) {
      this.stripeApiKey = await getStripeApiKey(runenv);
      this.lnUsername = await getLNUsername(runenv);
      this.lnPassword = await getLNPassword(runenv);
      this.stripe = new Stripe(this.stripeApiKey, {
        apiVersion: "2020-08-27",
      });
    }
    const lnConfig: ILexisNexisConfig = {
      username: this.lnUsername,
      password: this.lnPassword,
    };

    const committee = await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(
      createContributionInput.committeeId
    )(currentUser);

    validateContribOrThrow(createContributionInput);

    const {
      paymentMethod,
      cardCVC,
      cardNumber,
      cardExpirationMonth,
      cardExpirationYear,
    } = createContributionInput;

    if ([PaymentMethod.Credit, PaymentMethod.Debit].includes(paymentMethod)) {
      if (
        !cardCVC ||
        !cardNumber ||
        !cardExpirationMonth ||
        !cardExpirationYear
      )
        throw new ValidationError(
          "Card info must be provided for contributions in credit and debit"
        );
    }

    const res = await runRulesAndProcess(billableEventsTableName)(
      donorsTableName
    )(txnsTableName)(rulesTableName)(dynamoDB)(this.stripe)(lnConfig)(
      currentUser
    )(committee)(createContributionInput)();

    if (isLeft(res)) {
      throw res.left;
    } else {
      return res.right;
    }
  }

  @Mutation((returns) => Transaction)
  async createDisbursement(
    @Arg("createDisbursementData")
    d: CreateDisbursementInput,
    @CurrentUser() currentUser: string
  ): Promise<Transaction> {
    const committee = await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(
      d.committeeId
    )(currentUser);

    if (!this.lnUsername || !this.lnPassword) {
      this.lnUsername = await getLNUsername(runenv);
      this.lnPassword = await getLNPassword(runenv);
    }
    const lnConfig: ILexisNexisConfig = {
      username: this.lnUsername,
      password: this.lnPassword,
    };

    if (d.paymentMethod === PaymentMethod.Check && !d.checkNumber)
      throw new ValidationError(
        "Check number must be provided for check disbursements"
      );

    const res = await verifyAndCreateDisb(currentUser)(txnsTableName)(
      billableEventsTableName
    )(dynamoDB)(lnConfig)(committee)(d)();

    if (isLeft(res)) {
      throw res.left;
    } else {
      return res.right;
    }
  }

  @Mutation((returns) => Transaction)
  async amendDisbursement(
    @Arg("amendDisbursementData") d: AmendDisbInput,
    @CurrentUser() currentUser: string
  ) {
    await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(d.committeeId)(
      currentUser
    );

    const res = await amendDisb(txnsTableName)(dynamoDB)(d.committeeId)(
      d.transactionId
    )(d)();

    if (isLeft(res)) {
      throw res.left;
    } else {
      return res.right;
    }
  }

  @Mutation((returns) => Transaction)
  async amendContribution(
    @Arg("amendContributionData") c: AmendContributionInput,
    @CurrentUser() currentUser: string
  ) {
    await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(c.committeeId)(
      currentUser
    );

    validateContribOrThrow(c);

    const res = await amendContrib(txnsTableName)(dynamoDB)(c.committeeId)(
      c.transactionId
    )(c)();

    if (isLeft(res)) {
      throw res.left;
    } else {
      return res.right;
    }
  }

  @Mutation((returns) => Transaction)
  async reconcileTransaction(
    @Arg("reconcileTransactionData") rd: ReconcileTxnInput,
    @CurrentUser() currentUser: string
  ) {
    if (rd.selectedTransactions.length === 0)
      throw new ValidationError("Selected transactions list cannot be empty");

    await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(rd.committeeId)(
      currentUser
    );

    const res = await reconcileTxnWithTxns(txnsTableName)(dynamoDB)(
      rd.committeeId
    )(rd.bankTransaction)(rd.selectedTransactions)();

    if (isLeft(res)) {
      throw res.left;
    } else {
      return res.right;
    }
  }
}
