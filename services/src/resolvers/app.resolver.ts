import {
  Arg,
  Args,
  Mutation,
  Query,
  Resolver,
  UnauthorizedError,
} from "type-graphql";

import { Committee } from "../types/committee.type";
import { Transaction } from "../types/transaction.type";
import { Aggregations } from "../types/aggregations.type";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import * as dotenv from "dotenv";
import { searchTransactions } from "../queries/search-transactions.query";
import { TransactionsArg } from "../args/transactions.arg";
import { isLeft } from "fp-ts/Either";
import CurrentUser from "../decorators/current-user.decorator";
import { loadCommitteeOrThrow } from "../utils/model/load-committee-or-throw.utils";
import { CreateContributionInput } from "../input-types/create-contribution.input-type";
import {
  getFinicityAppKey,
  getFinicityPartnerId,
  getFinicityPartnerSecret,
  getLNPassword,
  getLNUsername,
  getStratoENodeUrl,
  getStratoNodeUrl,
  getStratoOAuthClientId,
  getStratoOauthClientSecret,
  getStratoOAuthOpenIdDiscoveryUrl,
  getStripeApiKey,
} from "../utils/config";
import { Stripe } from "stripe";
import { ValidationError } from "apollo-server-lambda";
import { PaymentMethod } from "../utils/enums/payment-method.enum";
import { CreateDisbursementInput } from "../input-types/create-disbursement.input-type";
import { runRulesAndProcess } from "../pipes/run-rules-and-process.pipe";
import * as https from "https";
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
import { Report } from "../types/report.type";
import { generateDisclosure } from "../pipes/generate-disclosure.pipe";
import { getAggsByCommitteeId } from "../utils/model/get-aggs.utils";
import { refreshAggs } from "../pipes/refresh-aggs.pipe";
import { GenCommitteeInput } from "../input-types/gen-committee.input-type";
import { initStratoConfig } from "../clients/dapp/dapp.decoders";
import { FinicityConfig } from "../clients/finicity/finicity.decoders";
import { genDemoCommittee } from "../demo/gen-committee.demo";
import { deleteUnreconciledTxn } from "../pipes/delete-txn.pipe";

const demoPasscode = "f4jp1i";
dotenv.config();

const billableEventsTableName: any = process.env.BILLABLE_EVENTS_DDB_TABLE_NAME;
const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;
const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const donorsTableName: any = process.env.DONORS_DDB_TABLE_NAME;
const rulesTableName: any = process.env.RULES_DDB_TABLE_NAME;
const aggTable: any = process.env.AGGREGATES_DDB_TABLE_NAME;
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

const ps = new AWS.SSM();

@Resolver()
export class AppResolver {
  private stripeApiKey: string;
  private stripe: Stripe;
  private lnUsername: string;
  private lnPassword: string;
  private nodeUrl: string;
  private eNodeUrl: string;
  private oauthClientId: string;
  private oauthClientSecret: string;
  private oauthOpenIdDiscoveryUrl: string;
  private finPartnerId: string;
  private finPartnerSecret: string;
  private finAppKey: string;
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

  @Query((returns) => Report)
  async report(
    @Arg("committeeId") committeeId: string,
    @CurrentUser() currentUser: string
  ) {
    const committee = await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(
      committeeId
    )(currentUser);

    const res = await searchTransactions(txnsTableName)(dynamoDB)({
      committeeId,
      bankVerified: true,
      ruleVerified: true,
    })();
    if (isLeft(res)) {
      throw res.left;
    } else {
      const txns = res.right;

      const csvData = await generateDisclosure(committee)(txns);
      return {
        csvData,
      };
    }
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

    const res = await getAggsByCommitteeId(aggTable)(dynamoDB)(committeeId)();
    if (isLeft(res)) {
      throw res.left;
    }

    const aggs = res.right;
    return aggs;
  }

  @Mutation((returns) => Transaction)
  async deleteTransaction(
    @Args() txnArg: TransactionArg,
    @CurrentUser() currentUser: string
  ): Promise<Transaction> {
    const committee = await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(
      txnArg.committeeId
    )(currentUser);

    const res = await deleteUnreconciledTxn(txnsTableName)(dynamoDB)(txnArg)();

    if (isLeft(res)) {
      throw res.left;
    } else {
      await refreshAggs(aggTable)(txnsTableName)(dynamoDB)(committee.id)();
      return res.right;
    }
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
      this.stripeApiKey = await getStripeApiKey(ps)(runenv);
      this.lnUsername = await getLNUsername(ps)(runenv);
      this.lnPassword = await getLNPassword(ps)(runenv);
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
      processPayment,
    } = createContributionInput;

    if (
      [PaymentMethod.Credit, PaymentMethod.Debit].includes(paymentMethod) &&
      processPayment
    ) {
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
      await refreshAggs(aggTable)(txnsTableName)(dynamoDB)(committee.id)();
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
      this.lnUsername = await getLNUsername(ps)(runenv);
      this.lnPassword = await getLNPassword(ps)(runenv);
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
      await refreshAggs(aggTable)(txnsTableName)(dynamoDB)(committee.id)();
      return res.right;
    }
  }

  @Mutation((returns) => Transaction)
  async amendDisbursement(
    @Arg("amendDisbursementData") d: AmendDisbInput,
    @CurrentUser() currentUser: string
  ) {
    const committee = await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(
      d.committeeId
    )(currentUser);

    const res = await amendDisb(txnsTableName)(dynamoDB)(currentUser)(
      d.committeeId
    )(d.transactionId)(d)();

    if (isLeft(res)) {
      throw res.left;
    } else {
      await refreshAggs(aggTable)(txnsTableName)(dynamoDB)(committee.id)();
      return res.right;
    }
  }

  @Mutation((returns) => Transaction)
  async amendContribution(
    @Arg("amendContributionData") c: AmendContributionInput,
    @CurrentUser() currentUser: string
  ) {
    const committee = await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(
      c.committeeId
    )(currentUser);

    validateContribOrThrow(c);

    const res = await amendContrib(txnsTableName)(dynamoDB)(currentUser)(
      c.committeeId
    )(c.transactionId)(c)();

    if (isLeft(res)) {
      throw res.left;
    } else {
      await refreshAggs(aggTable)(txnsTableName)(dynamoDB)(committee.id)();
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

    const committee = await loadCommitteeOrThrow(committeesTableName)(dynamoDB)(
      rd.committeeId
    )(currentUser);

    const res = await reconcileTxnWithTxns(txnsTableName)(dynamoDB)(
      rd.committeeId
    )(rd.bankTransaction)(rd.selectedTransactions)();

    if (isLeft(res)) {
      throw res.left;
    } else {
      await refreshAggs(aggTable)(txnsTableName)(dynamoDB)(committee.id)();
      return res.right;
    }
  }

  @Mutation((returns) => Committee)
  async generateCommittee(
    @Arg("genCommittee") c: GenCommitteeInput,
    @CurrentUser() currentUser: string
  ) {
    if (c.password !== demoPasscode || runenv === "prod")
      throw new UnauthorizedError();

    if (
      !this.nodeUrl ||
      !this.eNodeUrl ||
      !this.oauthClientId ||
      !this.oauthClientSecret ||
      !this.oauthOpenIdDiscoveryUrl ||
      !this.finPartnerId ||
      !this.finPartnerSecret ||
      !this.finAppKey
    ) {
      this.nodeUrl = await getStratoNodeUrl(ps)(runenv);
      this.eNodeUrl = await getStratoENodeUrl(ps)(runenv);
      this.oauthClientId = await getStratoOAuthClientId(ps)(runenv);
      this.oauthClientSecret = await getStratoOauthClientSecret(ps)(runenv);
      this.oauthOpenIdDiscoveryUrl = await getStratoOAuthOpenIdDiscoveryUrl(ps)(
        runenv
      );
      this.finPartnerId = await getFinicityPartnerId(ps)(runenv);
      this.finPartnerSecret = await getFinicityPartnerSecret(ps)(runenv);
      this.finAppKey = await getFinicityAppKey(ps)(runenv);
    }

    const stratoConf = initStratoConfig({
      nodeUrl: this.nodeUrl,
      eNodeUrl: this.eNodeUrl,
      oauthClientId: this.oauthClientId,
      oauthClientSecret: this.oauthClientSecret,
      oauthOpenIdDiscoveryUrl: this.oauthOpenIdDiscoveryUrl,
    });

    const finConf: FinicityConfig = {
      partnerId: this.finPartnerId,
      partnerSecret: this.finPartnerSecret,
      appKey: this.finAppKey,
    };

    const committee = await genDemoCommittee(committeesTableName)(
      txnsTableName
    )(dynamoDB)(finConf)(stratoConf);

    await refreshAggs(aggTable)(txnsTableName)(dynamoDB)(committee.id)();

    console.log("demo committee: ", committee);

    return committee;
  }
}
