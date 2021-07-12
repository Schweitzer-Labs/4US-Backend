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
import { TransactionType } from "../utils/enums/transaction-type.enum";
import { IInstantIdConfig } from "../clients/lexis-nexis/lexis-nexis.client";
import { getLNPassword, getLNUsername, getStripeApiKey } from "../utils/config";
import { Stripe } from "stripe";
import { EntityType } from "../utils/enums/entity-type.enum";
import { ValidationError } from "apollo-server-lambda";
import { PaymentMethod } from "../utils/enums/payment-method.enum";
import { CreateDisbursementInput } from "../input-types/create-disbursement.input-type";
import { createDisbursementInputToTransaction } from "../utils/model/create-disbursement-input-to-transaction.utils";
import { putTransaction } from "../utils/model/put-transaction.utils";
import { VerifyDisbursementInput } from "../input-types/verify-disbursement.input-type";
import { verifyDisbursementFromUserAndPut } from "../pipes/verify-disbursement-from-user.pipe";
import { Direction } from "../utils/enums/direction.enum";
import { runRulesAndProcess } from "../pipes/run-rules-and-process.pipe";
import * as https from "https";
import { txnsToAgg } from "../utils/model/txns-to-agg.utils";
import { TransactionArg } from "../args/transaction.arg";
import { getTxnById } from "../utils/model/get-txn-by-id.utils";
import { ReconcileDisbursementInput } from "../input-types/reconcile-disbursement.input-type";

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
    const instantIdConfig: IInstantIdConfig = {
      username: this.lnUsername,
      password: this.lnPassword,
    };

    const committee = await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(
      createContributionInput.committeeId
    )(currentUser);

    const {
      paymentMethod,
      entityType,
      entityName,
      cardCVC,
      cardNumber,
      cardExpirationMonth,
      cardExpirationYear,
      paymentDate,
      checkNumber,
    } = createContributionInput;

    if (![EntityType.Ind, EntityType.Fam].includes(entityType) && !entityName) {
      throw new ValidationError(
        "Entity name must be provided for non-individual and non-family contributions"
      );
    }
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

    if (paymentMethod === PaymentMethod.Check) {
      if (!checkNumber)
        throw new ValidationError(
          "Check number must be provided for contributions by check"
        );
    }

    if ([PaymentMethod.Check, PaymentMethod.Ach].includes(paymentMethod)) {
      if (!paymentDate)
        throw new ValidationError(
          "Payment date must be provided for contributions by check or ACH"
        );
    }

    const res = await runRulesAndProcess(billableEventsTableName)(
      donorsTableName
    )(txnsTableName)(rulesTableName)(dynamoDB)(this.stripe)(instantIdConfig)(
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
  ) {
    await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(d.committeeId)(
      currentUser
    );

    const txn = createDisbursementInputToTransaction(currentUser)(d);

    return await putTransaction(txnsTableName)(dynamoDB)(txn);
  }

  @Mutation((returns) => Transaction)
  async verifyDisbursement(
    @Arg("committeeId") committeeId: string,
    @Arg("transactionId") txnId: string,
    @Arg("verifyDisbursementData") d: VerifyDisbursementInput,
    @CurrentUser() currentUser: string
  ) {
    await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(committeeId)(
      currentUser
    );

    const res = await verifyDisbursementFromUserAndPut(txnsTableName)(dynamoDB)(
      committeeId
    )(txnId)(d)();

    if (isLeft(res)) {
      throw res.left;
    } else {
      return res.right;
    }
  }

  @Mutation((returns) => Transaction)
  async reconcileDisbursement(
    @Arg("committeeId") committeeId: string,
    @Arg("reconcileDisbursementData") rd: ReconcileDisbursementInput,
    @CurrentUser() currentUser: string
  ) {
    await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(committeeId)(
      currentUser
    );

    // const res = await verifyDisbursementFromUserAndPut(txnsTableName)(dynamoDB)(
    //   committeeId
    // )(txnId)(d)();
    //
    // if (isLeft(res)) {
    //   throw res.left;
    // } else {
    //   return res.right;
    // }
  }
}
