import { expect } from "chai";
import graphql from "../../src/committee-graphql.lambda";
import { genGraphQLProxy } from "../utils/gen-allowed-proxy.util";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { sleep } from "../../src/utils/sleep.utils";
import { genCommittee } from "../utils/gen-committee.util";
import { putCommittee } from "../../src/utils/model/put-committee.utils";
import * as dotenv from "dotenv";
import { qaUsers } from "../seed/qa-users.data";
import { deleteCommittee } from "../../src/utils/model/delete-committee.utils";
import { PaymentMethod } from "../../src/utils/enums/payment-method.enum";
import { genCreateContribInput } from "../utils/gen-create-contrib-input.util";
import { lambdaPromise } from "../../src/utils/lambda-promise.util";
import { genContributionRecord } from "../utils/gen-contribution.util";
import { putTransaction } from "../../src/utils/model/put-transaction.utils";
import { genCreateDisbInput } from "../utils/gen-create-disb-input.util";
import { genAmendDisbInput } from "../utils/gen-amend-disb-input.util";
import * as faker from "faker";
import { ITransaction } from "../../src/queries/search-transactions.decoder";
import { now, milliToEpoch } from "../../src/utils/time.utils";
import { genFinicityTxn } from "../utils/gen-finicity-txn.util";
import { Direction } from "../../src/utils/enums/direction.enum";
import { ReconcileTxnInput } from "../../src/graphql/input-types/reconcile-txn.input-type";
import { genAmendContribInput } from "../utils/get-amend-disb-input.util";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;
const txnsTableName: any = process.env.TRANSACTIONS_DDB_TABLE_NAME;

const validUsername = qaUsers[0];

const invalidUsername = "james-martin";

const expectedForbiddenText =
  "Access denied! You need to be authorized to perform this action!";

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

const getAllTransactionsQuery = `
  query {
    transactions(committeeId: "${committeeId}") {
      lastName
      firstName
      amount
      direction
    }
  }
`;

const getTransactionsByDonorIdQuery = (donorId: string) => `
  query {
    transactions(committeeId: "${committeeId}", donorId: "${donorId}") {
      donorId
      lastName
      firstName
      amount
      direction
    }
  }
`;

const getTxnQuery = (committeeId) => (tid: string) =>
  `
  query {
    transaction(committeeId: "${committeeId}", id: "${tid}") {
      id
      entityName
      addressLine1
      finicityCategory
      finicityBestRepresentation
      finicityPostedDate
      finicityTransactionDate
      finicityNormalizedPayeeName
      finicityDescription
      ruleVerified
      bankVerified
    }
  }
`;

const getCommitteeQuery = `
  query {
    committee(committeeId: "${committeeId}") {
      id
      candidateFirstName
    }
  }
`;

const aggregationsQuery = `
  query {
    aggregations(committeeId: "${committeeId}") {
      balance,
      totalRaised,
      totalSpent,
      totalDonors,
      totalTransactions,
      totalContributionsInProcessing,
      totalDisbursementsInProcessing,
      needsReviewCount
    }
  }
`;

const createDisb = `
  mutation(
      $committeeId: String!
      $amount: Float!
      $paymentMethod: PaymentMethod!
      $entityName: String!
      $addressLine1: String!
      $city: String!
      $state: State!
      $postalCode: String!
      $isSubcontracted: Boolean!
      $isPartialPayment: Boolean!
      $isExistingLiability: Boolean!
      $purposeCode: PurposeCode!
      $paymentDate: Float!
      $checkNumber: String
      $addressLine2: String
    ) {
      createDisbursement(createDisbursementData: {
        committeeId: $committeeId
        amount: $amount
        paymentMethod: $paymentMethod
        entityName: $entityName
        addressLine1: $addressLine1
        city: $city
        state: $state
        postalCode: $postalCode
        isSubcontracted: $isSubcontracted
        isPartialPayment: $isPartialPayment
        isExistingLiability: $isExistingLiability
        purposeCode: $purposeCode
        paymentDate: $paymentDate
        checkNumber: $checkNumber
        addressLine2: $addressLine2
      }) {
        id
      }
    }
`;

const amendDisbMut = `
    mutation (
      $committeeId: String!
      $transactionId: String!
      $entityName: String
      $addressLine1: String
      $addressLine2: String
      $city: String
      $state: State
      $postalCode: String
      $paymentDate: Float
      $checkNumber: String
      $purposeCode: PurposeCode
      $isExistingLiability: Boolean
      $isPartialPayment: Boolean
      $isSubContracted: Boolean
    ) {
      amendDisbursement(
        amendDisbursementData: {
          committeeId: $committeeId
          transactionId: $transactionId
          entityName: $entityName
          addressLine1: $addressLine1
          addressLine2: $addressLine2
          city: $city
          state: $state
          postalCode: $postalCode
          paymentDate: $paymentDate
          checkNumber: $checkNumber
          purposeCode: $purposeCode
          isExistingLiability: $isExistingLiability
          isPartialPayment: $isPartialPayment
          isSubcontracted: $isSubContracted
        }
      ) {
        id
      }
    }
`;

const createContribMut = `
mutation(
      $committeeId: String!
      $amount: Float!
      $paymentMethod: PaymentMethod!
      $firstName: String!
      $lastName: String!
      $addressLine1: String!
      $city: String!
      $state: State!
      $postalCode: String!
      $entityType: EntityType!
      $emailAddress: String
      $paymentDate: Float!
      $cardNumber: String
      $cardExpirationMonth: Float
      $cardExpirationYear: Float
      $cardCVC: String
      $checkNumber: String
      $entityName: String
      $employer: String
      $occupation: String
      $middleName: String
      $refCode: String
      $processPayment: Boolean!
    ) {
      createContribution(createContributionData: {
        committeeId: $committeeId
        amount: $amount
        paymentMethod: $paymentMethod
        firstName: $firstName
        lastName: $lastName
        addressLine1: $addressLine1
        city: $city
        state: $state
        postalCode: $postalCode
        entityType: $entityType
        emailAddress: $emailAddress
        paymentDate: $paymentDate
        cardNumber: $cardNumber
        cardExpirationMonth: $cardExpirationMonth
        cardExpirationYear: $cardExpirationYear
        cardCVC: $cardCVC
        checkNumber: $checkNumber
        entityName: $entityName
        employer: $employer
        occupation: $occupation
        middleName: $middleName
        refCode: $refCode
        processPayment: $processPayment
      }) {
        id
        amount
      }
    }
`;

const amendContribMut = `
  mutation(
      $committeeId: String!
      $transactionId: String!
      $amount: Float
      $paymentMethod: PaymentMethod
      $firstName: String
      $lastName: String
      $addressLine1: String
      $city: String
      $state: State
      $postalCode: String
      $entityType: EntityType
      $emailAddress: String
      $paymentDate: Float
      $checkNumber: String
      $entityName: String
      $employer: String
      $occupation: String
      $middleName: String
      $refCode: String
      $addressLine2: String
      $companyName: String
      $phoneNumber: String
      $attestsToBeingAnAdultCitizen: Boolean
      $employmentStatus: EmploymentStatus
    ) {
      amendContribution(
        amendContributionData: {
          committeeId: $committeeId
          transactionId: $transactionId
          amount: $amount
          paymentMethod: $paymentMethod
          firstName: $firstName
          lastName: $lastName
          addressLine1: $addressLine1
          city: $city
          state: $state
          postalCode: $postalCode
          entityType: $entityType
          emailAddress: $emailAddress
          paymentDate: $paymentDate
          checkNumber: $checkNumber
          entityName: $entityName
          employer: $employer
          occupation: $occupation
          middleName: $middleName
          refCode: $refCode
          addressLine2: $addressLine2
          companyName: $companyName
          phoneNumber: $phoneNumber
          attestsToBeingAnAdultCitizen: $attestsToBeingAnAdultCitizen
          employmentStatus: $employmentStatus
        }
      ) {
        id
        amount
      }
    }
`;

const recTxnMutation = `
    mutation(
      $committeeId: String!,
      $selectedTransactions: [String!]!,
      $bankTransaction: String!
    ) {
      reconcileTransaction(
        reconcileTransactionData: {
            selectedTransactions: $selectedTransactions,
            bankTransaction: $bankTransaction,
            committeeId: $committeeId
        }
      ) {
        id
      }
    }
`;

const deleteTxnMut = `
  mutation(
    $id: String!
    $committeeId: String!
  ) {
    deleteTransaction(
      id: $id
      committeeId: $committeeId
    ) {
      amount
    }
  }
`;

describe("Committee GraphQL Lambda", function () {
  before(async () => {
    await putCommittee(committeesTableName)(dynamoDB)(committee);
    await sleep(1000);
  });
  describe("Permissions", function () {
    it("Prevents a non-member user from querying a committee", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getCommitteeQuery, invalidUsername),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.errors[0].message).to.equal(expectedForbiddenText);
    });
    it("Prevents a non-member user from querying a transaction", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getAllTransactionsQuery, invalidUsername),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.errors[0].message).to.equal(expectedForbiddenText);
    });
    it("Prevents a non-member user from querying an aggregation", async () => {
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(aggregationsQuery, invalidUsername),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.errors[0].message).to.equal(expectedForbiddenText);
    });
  });
  describe("Transactions", function () {
    it("Get by Committee ID", async () => {
      const txn = genContributionRecord(committeeId);
      await putTransaction(txnsTableName)(dynamoDB)(txn);
      await sleep(1000);
      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(getAllTransactionsQuery, validUsername),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.data.transactions.length > 0).to.equal(true);
    });
    it("Get by Committee ID and Donor ID", async () => {
      const txn = genContributionRecord(committeeId);
      await putTransaction(txnsTableName)(dynamoDB)(txn);
      await sleep(1000);

      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(
          getTransactionsByDonorIdQuery(txn.donorId),
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
        genGraphQLProxy(getCommitteeQuery, validUsername),
        {}
      );
      const body = JSON.parse(res.body);
      expect(body.data.committee.id).to.equal(committee.id);
    });
  });
  describe("Aggregations", function () {
    it("Get by Committee ID", async () => {
      const query = genGraphQLProxy(aggregationsQuery, validUsername);
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
        "Get Transaction by ID: Not Found"
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
  });
  after(async () => {
    await deleteCommittee(committeesTableName)(dynamoDB)(committee);
  });
});
