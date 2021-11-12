import {
  Arg,
  Args,
  Mutation,
  Query,
  Resolver,
  UnauthorizedError,
} from "type-graphql";

import { Committee } from "./object-types/committee.object-type";
import { Transaction } from "./object-types/transaction.object-type";
import { Aggregations } from "./object-types/aggregations.object-type";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import * as dotenv from "dotenv";
import { searchTransactions } from "../utils/model/transaction/search-transactions.query";
import { TransactionsArg } from "./args/transactions.arg";
import { isLeft } from "fp-ts/Either";
import CurrentUser from "./decorators/current-user.decorator";
import {
  loadCommitteeOrError,
  loadCommitteeOrThrow,
} from "../utils/model/committee/load-committee-or-throw.utils";
import { CreateContributionInput } from "./input-types/create-contribution.input-type";
import { taskEither as te } from "fp-ts";
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
import { CreateDisbursementInput } from "./input-types/create-disbursement.input-type";
import { runRulesAndProcess } from "../pipes/run-rules-and-process.pipe";
import * as https from "https";
import { TransactionArg } from "./args/transaction.arg";
import { getTxnById } from "../utils/model/transaction/get-txn-by-id.utils";
import { ReconcileTxnInput } from "./input-types/reconcile-txn.input-type";
import { AmendDisbInput } from "./input-types/amend-disb.input-type";
import { amendDisb } from "../pipes/amend-disb.pipe";
import { AmendContributionInput } from "./input-types/amend-contrib.input-type";
import { amendContrib } from "../pipes/amend-contrib.pipe";
import {
  validateContrib,
  validateContribOrThrowGQLError,
} from "./validators/contrib.validator";
import { reconcileTxnWithTxns } from "../pipes/reconcile-txn.pipe";
import { ILexisNexisConfig } from "../clients/lexis-nexis/lexis-nexis.client";
import { verifyAndCreateDisb } from "../pipes/verify-and-create-disb.pipe";
import { Report } from "./object-types/report.object-type";
import { generateDisclosureOrError } from "../pipes/generate-disclosure.pipe";
import { getAggsByCommitteeId } from "../utils/model/aggs/get-aggs.utils";
import { refreshAggs, refreshAggsFromTxn } from "../pipes/refresh-aggs.pipe";
import { GenCommitteeInput } from "./input-types/gen-committee.input-type";
import { initStratoConfig } from "../clients/dapp/dapp.decoders";
import { FinicityConfig } from "../clients/finicity/finicity.decoders";
import { genDemoCommittee } from "../demo/gen-committee.demo";
import { deleteUnreconciledTxn } from "../pipes/delete-txn.pipe";
import { ManageDemoCommitteeInput } from "./input-types/manage-demo-committee.input-type";
import { reconcileOneDemoContrib } from "../demo/utils/reconcile-one-demo-contrib.util";
import { SeedDemoBankRecordsInput } from "./input-types/seed-demo-bank-records.input-type";
import { seedTxn } from "../demo/utils/seed-bank-records.util";
import { pipe } from "fp-ts/function";
import { teToRightOrThrow } from "../utils/te-to-right-or-throw.util";
import { validateCard } from "./validators/card.validators";
import { validateCheck } from "./validators/check.validators";
import { validateReconcileInput } from "./validators/reconcile.validators";
import { ITransaction } from "../model/transaction.type";

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

const ddb = new DynamoDB({
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
    const res = pipe(
      loadCommitteeOrError(committeesTableName)(ddb)(committeeId)(currentUser),
      te.mapLeft((appErr) => new ValidationError(appErr.message))
    );

    return await teToRightOrThrow(res);
  }

  @Query((returns) => Report)
  async report(
    @Arg("committeeId") committeeId: string,
    @Arg("includeHeaders") includeHeaders: boolean = false,
    @CurrentUser() currentUser: string
  ) {
    const res = pipe(
      loadCommitteeOrError(committeesTableName)(ddb)(committeeId)(currentUser),
      te.chain((committee) =>
        pipe(
          searchTransactions(txnsTableName)(ddb)({
            committeeId,
            ruleVerified: true,
            bankVerified: true,
          }),
          te.chain(generateDisclosureOrError(committee)(includeHeaders)),
          te.map((csvData) => ({
            csvData,
          }))
        )
      ),
      te.mapLeft((appErr) => new ValidationError(appErr.message))
    );
    return await teToRightOrThrow(res);
  }

  @Query((returns) => [Transaction])
  async transactions(
    @Args() txnArgs: TransactionsArg,
    @CurrentUser() currentUser: string
  ): Promise<Transaction[]> {
    const res = pipe(
      loadCommitteeOrError(committeesTableName)(ddb)(txnArgs.committeeId)(
        currentUser
      ),
      te.chain(() => searchTransactions(txnsTableName)(ddb)(txnArgs)),
      te.mapLeft((appErr) => new ValidationError(appErr.message))
    );
    return await teToRightOrThrow(res);
  }

  @Query((returns) => Transaction, { nullable: true })
  async transaction(
    @Args() txnArg: TransactionArg,
    @CurrentUser() currentUser: string
  ): Promise<Transaction> {
    const res = pipe(
      loadCommitteeOrError(committeesTableName)(ddb)(txnArg.committeeId)(
        currentUser
      ),
      te.chain(() =>
        getTxnById(txnsTableName)(ddb)(txnArg.committeeId)(txnArg.id)
      ),
      te.mapLeft((appErr) => new ValidationError(appErr.message))
    );
    return await teToRightOrThrow(res);
  }

  @Query((returns) => Aggregations)
  async aggregations(
    @Arg("committeeId") committeeId: string,
    @CurrentUser() currentUser: string
  ): Promise<Aggregations> {
    const res = pipe(
      loadCommitteeOrError(committeesTableName)(ddb)(committeeId)(currentUser),
      te.chain(() => getAggsByCommitteeId(aggTable)(ddb)(committeeId)),
      te.mapLeft((appErr) => new ValidationError(appErr.message))
    );
    return await teToRightOrThrow(res);
  }

  @Mutation((returns) => Transaction)
  async deleteTransaction(
    @Args() txnArg: TransactionArg,
    @CurrentUser() currentUser: string
  ): Promise<Transaction> {
    const res = pipe(
      loadCommitteeOrError(committeesTableName)(ddb)(txnArg.committeeId)(
        currentUser
      ),
      te.chain(() => deleteUnreconciledTxn(txnsTableName)(ddb)(txnArg)),
      te.chain((txn: ITransaction) =>
        pipe(
          refreshAggsFromTxn(aggTable)(txnsTableName)(ddb)(txn),
          te.map(() => txn)
        )
      ),
      te.mapLeft((appErr) => new ValidationError(appErr.message))
    );
    return await teToRightOrThrow(res);
  }

  @Mutation((returns) => Transaction)
  async createContribution(
    @Arg("createContributionData")
    contribInput: CreateContributionInput,
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

    const res = pipe(
      validateCard(contribInput),
      te.chain(() =>
        loadCommitteeOrError(committeesTableName)(ddb)(
          contribInput.committeeId
        )(currentUser)
      ),
      te.chain((committee) =>
        pipe(
          validateContrib(committee)(contribInput),
          te.chain(() =>
            runRulesAndProcess(false)(billableEventsTableName)(donorsTableName)(
              txnsTableName
            )(rulesTableName)(ddb)(this.stripe)(lnConfig)(currentUser)(
              committee
            )(contribInput)
          )
        )
      ),
      te.chain((txn: ITransaction) =>
        pipe(
          refreshAggsFromTxn(aggTable)(txnsTableName)(ddb)(txn),
          te.map(() => txn)
        )
      ),
      // @ToDo add custom error to show balances and other metadata
      te.mapLeft((appErr) => new ValidationError(appErr.message))
    );

    return await teToRightOrThrow(res);
  }

  @Mutation((returns) => Transaction)
  async createDisbursement(
    @Arg("createDisbursementData")
    d: CreateDisbursementInput,
    @CurrentUser() currentUser: string
  ): Promise<Transaction> {
    if (!this.lnUsername || !this.lnPassword) {
      this.lnUsername = await getLNUsername(ps)(runenv);
      this.lnPassword = await getLNPassword(ps)(runenv);
    }
    const lnConfig: ILexisNexisConfig = {
      username: this.lnUsername,
      password: this.lnPassword,
    };
    const res = pipe(
      validateCheck(d),
      te.chain(() =>
        loadCommitteeOrError(committeesTableName)(ddb)(d.committeeId)(
          currentUser
        )
      ),
      te.chain((committee) =>
        verifyAndCreateDisb(currentUser)(txnsTableName)(
          billableEventsTableName
        )(ddb)(lnConfig)(committee)(d)
      ),
      te.chain((txn) =>
        pipe(
          refreshAggsFromTxn(aggTable)(txnsTableName)(ddb)(txn),
          te.map(() => txn)
        )
      ),
      te.mapLeft((appErr) => new ValidationError(appErr.message))
    );
    return await teToRightOrThrow(res);
  }

  @Mutation((returns) => Transaction)
  async amendDisbursement(
    @Arg("amendDisbursementData") d: AmendDisbInput,
    @CurrentUser() currentUser: string
  ) {
    const res = pipe(
      loadCommitteeOrError(committeesTableName)(ddb)(d.committeeId)(
        currentUser
      ),
      te.chain(() =>
        amendDisb(txnsTableName)(ddb)(currentUser)(d.committeeId)(
          d.transactionId
        )(d)
      ),
      te.mapLeft((appErr) => new ValidationError(appErr.message))
    );
    return await teToRightOrThrow(res);
  }

  @Mutation((returns) => Transaction)
  async amendContribution(
    @Arg("amendContributionData") c: AmendContributionInput,
    @CurrentUser() currentUser: string
  ) {
    const committee = await loadCommitteeOrThrow(committeesTableName)(ddb)(
      c.committeeId
    )(currentUser);

    await validateContribOrThrowGQLError(committee)(c);

    const res = pipe(
      loadCommitteeOrError(committeesTableName)(ddb)(c.committeeId)(
        currentUser
      ),
      te.chain((committee) =>
        pipe(
          validateContrib(committee)(c),
          te.chain(() =>
            amendContrib(txnsTableName)(ddb)(currentUser)(c.committeeId)(
              c.transactionId
            )(c)
          )
        )
      ),
      te.mapLeft((appErr) => new ValidationError(appErr.message))
    );
    return await teToRightOrThrow(res);
  }

  @Mutation((returns) => Transaction)
  async reconcileTransaction(
    @Arg("reconcileTransactionData") r: ReconcileTxnInput,
    @CurrentUser() currentUser: string
  ) {
    const res = pipe(
      validateReconcileInput(r),
      te.chain(() =>
        loadCommitteeOrError(committeesTableName)(ddb)(r.committeeId)(
          currentUser
        )
      ),
      te.chain(() =>
        reconcileTxnWithTxns(txnsTableName)(ddb)(r.committeeId)(
          r.bankTransaction
        )(r.selectedTransactions)
      ),
      te.chain((txn) =>
        pipe(
          refreshAggsFromTxn(aggTable)(txnsTableName)(ddb)(txn),
          te.map(() => txn)
        )
      ),
      te.mapLeft((appErr) => new ValidationError(appErr.message))
    );

    return await teToRightOrThrow(res);
  }

  // Demo Generation Code

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
    )(ddb)(finConf)(stratoConf)(c);

    await refreshAggs(aggTable)(txnsTableName)(ddb)(committee.id)();

    return committee;
  }

  @Mutation((returns) => Transaction)
  async seedDemoBankRecords(
    @Arg("seedDemoBankRecordsInput") s: SeedDemoBankRecordsInput,
    @CurrentUser() currentUser: string
  ) {
    if (s.password !== demoPasscode || runenv === "prod")
      throw new UnauthorizedError();

    const res = await seedTxn(txnsTableName)(ddb)(s)();

    if (isLeft(res)) throw res.left;

    await refreshAggs(aggTable)(txnsTableName)(ddb)(s.committeeId)();

    return res.right;
  }

  @Mutation((returns) => Transaction)
  async reconcileOneDemoTransaction(
    @Arg("manageDemoCommitteeInput") d: ManageDemoCommitteeInput,
    @CurrentUser() currentUser: string
  ) {
    if (d.password !== demoPasscode || runenv === "prod")
      throw new UnauthorizedError();

    const res = await reconcileOneDemoContrib(txnsTableName)(ddb)(
      d.committeeId
    )();

    if (isLeft(res)) throw res.left;

    await refreshAggs(aggTable)(txnsTableName)(ddb)(d.committeeId)();

    return res.right;
  }
}
