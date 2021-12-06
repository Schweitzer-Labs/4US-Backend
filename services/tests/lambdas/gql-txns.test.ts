import { expect } from "chai";
import graphql from "../../src/committee-graphql.lambda";
import { genGraphQLProxy } from "../utils/gen-allowed-proxy.util";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { sleep } from "../../src/utils/sleep.utils";
import { genCommittee } from "../utils/gen-committee.util";
import { putCommittee } from "../../src/utils/model/committee/put-committee.utils";
import * as dotenv from "dotenv";
import { qaUsers } from "../seed/qa-users.data";
import { deleteCommittee } from "../../src/utils/model/committee/delete-committee.utils";
import { PaymentMethod } from "../../src/utils/enums/payment-method.enum";
import { genCreateContribInput } from "../utils/gen-create-contrib-input.util";
import { lambdaPromise } from "../../src/utils/lambda-promise.util";
import { genContributionRecord } from "../utils/gen-contribution.util";
import { putTransaction } from "../../src/utils/model/transaction/put-transaction.utils";
import { genCreateDisbInput } from "../utils/gen-create-disb-input.util";
import { genAmendDisbInput } from "../utils/gen-amend-disb-input.util";
import * as faker from "faker";
import { ITransaction } from "../../src/model/transaction.type";
import { now, milliToEpoch } from "../../src/utils/time.utils";
import { genFinicityTxn } from "../utils/gen-finicity-txn.util";
import { Direction } from "../../src/utils/enums/direction.enum";
import { ReconcileTxnInput } from "../../src/graphql/input-types/reconcile-txn.input-type";
import { genAmendContribInput } from "../utils/get-amend-disb-input.util";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";
import { Source } from "../../src/utils/enums/source.enum";
import {
  aggregationsQuery,
  amendContribMut,
  amendDisbMut,
  createContribMut,
  createDisb,
  deleteTxnMut,
  getAllTransactionsQuery,
  getCommitteeQuery,
  getTransactionsByDonorIdQuery,
  getTxnQuery,
  recTxnMutation,
} from "../utils/graphql.utils";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;

const validUsername = qaUsers[0];

const invalidUsername = "james-martin";

// const expectedForbiddenText =
//   "Access denied! You need to be authorized to perform this action!";

const expectedForbiddenText = "API use is not authorized";

const committee = genCommittee({
  district: "53",
  officeType: "senate",
  party: "democrat",
  race: "primary",
  ruleVersion: "nyboe-2020",
  scope: "state",
  state: "ny",
});

const committeeId = committee.id;

describe("Committee GraphQL Lambda", function () {
  before(async () => {
    await putCommittee(committeesTableName)(dynamoDB)(committee);
    await sleep(1000);
  });
  describe("Permissions", function () {
    it("Prevents a non-member user from querying a committee", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getCommitteeQuery(committeeId), invalidUsername),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.errors[0].message).to.equal(expectedForbiddenText);
    });
    it("Prevents a non-member user from querying a transaction", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getAllTransactionsQuery(committeeId), invalidUsername),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.errors[0].message).to.equal(expectedForbiddenText);
    });
    it("Prevents a non-member user from querying an aggregation", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(aggregationsQuery(committeeId), invalidUsername),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.errors[0].message).to.equal(expectedForbiddenText);
    });
  });
  describe("Transactions", function () {
    it("Get by Committee ID", async () => {
      const txn = genContributionRecord({ committeeId });
      await putTransaction(txnsTableName)(dynamoDB)(txn);
      await sleep(1000);
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getAllTransactionsQuery(committeeId), validUsername),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.data.transactions.length > 0).to.equal(true);
    });
    it("Get by Committee ID and Donor ID", async () => {
      const txn = genContributionRecord({ committeeId });
      await putTransaction(txnsTableName)(dynamoDB)(txn);
      await sleep(1000);

      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(
          getTransactionsByDonorIdQuery(committeeId)(txn.donorId),
          validUsername
        ),
        {}
      );
      const body: any = JSON.parse(res.body);
      expect(body.data.transactions[0].donorId).to.equal(txn.donorId);
    });
  });
  describe("Committee", function () {
    it("Get by Committee ID", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getCommitteeQuery(committeeId), validUsername),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.data.committee.id).to.equal(committee.id);
    });
  });
  describe("Aggregations", function () {
    it("Get by Committee ID", async () => {
      const query = genGraphQLProxy(
        aggregationsQuery(committeeId),
        validUsername
      );
      const res: any = await lambdaPromise(graphql, query, {});

      const body = JSON.parse(res.body);
      expect(body.data.aggregations.balance).to.be.a("number");
    });
  });

  describe("Create Contributions", function () {
    it("Supports the creation of a contribution", async () => {
      const vars = genCreateContribInput({ committeeId });
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(createContribMut, validUsername, vars),
        {}
      );

      const body = JSON.parse(res.body);
      expect(body.data.createContribution.amount).to.equal(vars.amount);
    });
    it("Rejects a faulty State value", async () => {
      const inputVar = { ...genCreateContribInput({ committeeId }), state: "" };

      const createRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(createContribMut, validUsername, inputVar),
        {}
      );

      const body = JSON.parse(createRes.body);

      expect(body.errors.length > 0).to.equal(true);
    });
  });
  describe("Create Disbursement", function () {
    it("Supports the creation of a disbursement", async () => {
      const inputVar = genCreateDisbInput({
        committeeId,
      });

      const createRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(createDisb, validUsername, inputVar),
        {}
      );

      const body = JSON.parse(createRes.body);

      const tid = body.data.createDisbursement.id;

      const txnRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getTxnQuery(committee.id)(tid), validUsername, {}),
        {}
      );

      const txnResBody = JSON.parse(txnRes.body);

      expect(txnResBody.data.transaction.id).to.equal(tid);
    });
    it("Rejects a check disbursement missing a check number", async () => {
      const inputVar = genCreateDisbInput({
        committeeId,
        paymentMethod: PaymentMethod.Check,
      });

      const createRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(createDisb, validUsername, inputVar),
        {}
      );
      console.log(createRes);

      const resBody = JSON.parse(createRes.body);

      expect(resBody.errors.length > 0).to.equal(true);
    });
    it("Exposes Business verification score ", async () => {
      const inputVar = genCreateDisbInput({
        committeeId,
      });

      const createRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(createDisb, validUsername, inputVar),
        {}
      );

      const body = JSON.parse(createRes.body);

      const tid = body.data.createDisbursement.id;

      const txnRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getTxnQuery(committee.id)(tid), validUsername, {}),
        {}
      );

      const txnResBody = JSON.parse(txnRes.body);

      const businessIdVerificationScore =
        txnResBody.data.transaction.businessIdVerificationScore;

      expect(isNaN(businessIdVerificationScore)).to.equal(false);
    });
  });
  describe("Amend Disbursement", function () {
    it("Supports amending a disbursement", async () => {
      // Create Disb
      const createInputVar = genCreateDisbInput({
        committeeId,
      });

      const createRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(createDisb, validUsername, createInputVar),
        {}
      );

      const body = JSON.parse(createRes.body);

      const tid = body.data.createDisbursement.id;

      // Amend Disb
      await sleep(1000);

      const amendInput = genAmendDisbInput({
        committeeId: committee.id,
        transactionId: tid,
        entityName: faker.name.lastName(),
      });

      const amendRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(amendDisbMut, validUsername, amendInput),
        {}
      );

      const amendResBody = JSON.parse(amendRes.body);

      console.log(amendResBody);

      await sleep(1000);

      // Get Disb

      const getTxnRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getTxnQuery(committee.id)(tid), validUsername, {}),
        {}
      );

      const txnResBody = JSON.parse(getTxnRes.body);

      console.log(amendResBody);

      expect(txnResBody.data.transaction.entityName).to.equal(
        amendInput.entityName
      );
    });
  });
  describe("Reconcile Transactions", function () {
    let bankTxn: ITransaction;
    let selectTxnId: string;

    before(async () => {
      const paymentDate = now();
      const paymentMethod = PaymentMethod.Debit;
      // Create bank txn
      bankTxn = genFinicityTxn({
        paymentDate,
        direction: Direction.Out,
        paymentMethod,
        committeeId,
      });

      await putTransaction(txnsTableName)(dynamoDB)(bankTxn);

      // Create Disb
      const createInputVar = genCreateDisbInput({
        committeeId,
        amount: bankTxn.amount,
        paymentDate,
        paymentMethod,
      });

      const createRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(createDisb, validUsername, createInputVar),
        {}
      );

      const createBody = JSON.parse(createRes.body);

      selectTxnId = createBody.data.createDisbursement.id;

      // Reconcile disb with bank txn

      const recDisbVars: ReconcileTxnInput = {
        committeeId,
        bankTransaction: bankTxn.id,
        selectedTransactions: [selectTxnId],
      };

      const recRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(recTxnMutation, validUsername, recDisbVars),
        {}
      );
    });
    it("Deletes the matching bank transaction", async () => {
      const getTxnRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(
          getTxnQuery(committee.id)(bankTxn.id),
          validUsername,
          {}
        ),
        {}
      );

      const txnResBody = JSON.parse(getTxnRes.body);
      expect(txnResBody.errors.length > 0).to.equal(true);
    });
    it("Verified matching transaction and attaches bank data", async () => {
      const getTxnRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(
          getTxnQuery(committeeId)(selectTxnId),
          validUsername,
          {}
        ),
        {}
      );

      const txnResBody = JSON.parse(getTxnRes.body);

      console.log("next one", txnResBody);

      const data = txnResBody.data.transaction;

      expect(data.finicityBestRepresentation).to.equal(
        bankTxn.finicityBestRepresentation
      );
      expect(data.finicityNormalizedPayeeName).to.equal(
        bankTxn.finicityNormalizedPayeeName
      );
      expect(data.finicityDescription).to.equal(bankTxn.finicityDescription);
      expect(data.ruleVerified).to.equal(true);
      expect(data.bankVerified).to.equal(true);
    });
  });
  describe("Amend Contribution", function () {
    it("Supports amending a contribution", async () => {
      // Create Contrib
      const createInputVar = genCreateContribInput({
        committeeId: committee.id,
      });

      const createRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(createContribMut, validUsername, createInputVar),
        {}
      );

      const body = JSON.parse(createRes.body);
      const tid = body.data.createContribution.id;

      // Amend Disb

      const amendInput = genAmendContribInput({
        committeeId: committee.id,
        transactionId: tid,
        addressLine1: faker.address.streetAddress(),
      });

      const amendRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(amendContribMut, validUsername, amendInput),
        {}
      );

      const amendResBody = JSON.parse(amendRes.body);

      // Get Contrib

      const getTxnRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getTxnQuery(committee.id)(tid), validUsername, {}),
        {}
      );

      const txnResBody = JSON.parse(getTxnRes.body);

      console.log(amendResBody);

      expect(txnResBody.data.transaction.addressLine1).to.equal(
        amendInput.addressLine1
      );
    });
  });
  describe("Transaction", function () {
    it("Gets a transaction by id and committeeId", async () => {
      const createRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(
          createContribMut,
          validUsername,
          genCreateContribInput({ committeeId })
        ),
        {}
      );

      const body = JSON.parse(createRes.body);

      const tid = body.data.createContribution.id;

      const txnRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getTxnQuery(committee.id)(tid), validUsername, {}),
        {}
      );

      const txnResBody = JSON.parse(txnRes.body);

      expect(txnResBody.data.transaction.id).to.equal(tid);
    });
    it("Gets a 404 on bad txn id", async () => {
      const tid = genTxnId();

      const txnRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getTxnQuery(committee.id)(tid), validUsername, {}),
        {}
      );

      const txnResBody = JSON.parse(txnRes.body);
      expect(txnResBody.data.transaction).to.equal(null);
    });
  });
  describe("Supports deleting unreconciled transactions", function () {
    it("Deletes an unreconciled and unprocessed transaction", async () => {
      const createRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(
          createContribMut,
          validUsername,
          genCreateContribInput({
            committeeId,
            paymentMethod: PaymentMethod.Check,
            checkNumber: "123",
          })
        ),
        {}
      );

      const body = JSON.parse(createRes.body);

      const id = body.data.createContribution.id;

      await lambdaPromise(
        graphql,
        genGraphQLProxy(deleteTxnMut, validUsername, {
          committeeId,
          id,
        }),
        {}
      );

      const txnRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getTxnQuery(committee.id)(id), validUsername, {}),
        {}
      );

      const txnResBody = JSON.parse(txnRes.body);
      expect(txnResBody.errors[0].message).to.equal(
        `Get Transaction by ID: Invalid ID ${id}`
      );
    });
    it("Stops a processed transaction from deletion", async () => {
      const createRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(
          createContribMut,
          validUsername,
          genCreateContribInput({ committeeId })
        ),
        {}
      );

      const body = JSON.parse(createRes.body);

      const id = body.data.createContribution.id;

      const txnRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(deleteTxnMut, validUsername, {
          committeeId,
          id,
        }),
        {}
      );

      const txnResBody = JSON.parse(txnRes.body);
      console.log(txnResBody);
      expect(txnResBody.errors[0].message).to.equal(
        "Transaction is not unreconciled or unprocessed."
      );
    });
    it("Stops a External Source transaction from deletion", async () => {
      const source = Source.ActBlue;
      const newTxn = genContributionRecord({ committeeId, source });

      await putTransaction(txnsTableName)(dynamoDB)(newTxn);
      await sleep(1000);

      const id = newTxn.id;

      const txnRes: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(deleteTxnMut, validUsername, {
          committeeId,
          id,
        }),
        {}
      );

      const txnResBody = JSON.parse(txnRes.body);
      console.log(txnResBody);
      expect(txnResBody.errors[0].message).to.equal(
        `${newTxn.source} transactions cannot be deleted.`
      );
    });
  });
  after(async () => {
    await deleteCommittee(committeesTableName)(dynamoDB)(committee);
  });
});
