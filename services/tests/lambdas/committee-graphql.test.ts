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
import { now } from "../../src/utils/time.utils";
import {
  genCreateDisbInput,
  genDisbInput,
} from "../utils/gen-create-disb-input.util";
import { deleteCommittee } from "../../src/utils/model/delete-committee.utils";
import { PaymentMethod } from "../../src/utils/enums/payment-method.enum";
import { genAmendDisbInput } from "../utils/gen-amend-disb-input.util";
import * as faker from "faker";

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
      $state: String!
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
      $state: String
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

const createContributionVariables = {
  committeeId,
};

const generalVariables = {
  committeeId,
};

const createContributionQuery = `
mutation($committeeId: String!) {
  createContribution(createContributionData: {
    committeeId: $committeeId
    amount: 12000
    paymentMethod: Credit
    cardCVC: "321"
    cardNumber: "4242424242424242"
    cardExpirationMonth: 12
    cardExpirationYear: 2023
    emailAddress: "dev.evanpiro@gmail.com"
    firstName: "Carvin"
    lastName: "Brierson"
    addressLine1: "43 Lane"
    addressLine2: "2FL"
    city: "Downingtown"
    state: "PA"
    postalCode: "13224"
    entityType: Ind
    employmentStatus: Unemployed
    paymentDate: ${now()}
  }) {
    amount
    id
  }
}
`;

const recDisbMutation = `
mutation($committeeId: String!) {
  reconcileDisbursement(reconcileDisbursementData: {
    committeeId: $committeeId,
    bankTransaction: "asdfsdaf",
    selectedTransactions: []
  }) {
    amount
    id
  }
}
`;

const recDisbVar = {};

const lambdaPromise = (lambda, event, context) => {
  return new Promise((resolve, reject) => {
    lambda(event, context, (error, res) => {
      resolve(res);
    });
  });
};

describe("Committee GraphQL Lambda", function () {
  before(async () => {
    await putCommittee(committeesTableName)(dynamoDB)(committee);
    await sleep(1000);
  });
  // describe("Permissions", function () {
  //   it("Prevents a non-member user from querying a committee", async () => {
  //     const res: any = await lambdaPromise(
  //       graphql,
  //       genGraphQLProxy(getCommitteeQuery, invalidUsername),
  //       {}
  //     );
  //     const body = JSON.parse(res.body);
  //     expect(body.errors[0].message).to.equal(expectedForbiddenText);
  //   });
  //   it("Prevents a non-member user from querying a transaction", async () => {
  //     const res: any = await lambdaPromise(
  //       graphql,
  //       genGraphQLProxy(getAllTransactionsQuery, invalidUsername),
  //       {}
  //     );
  //     const body = JSON.parse(res.body);
  //     expect(body.errors[0].message).to.equal(expectedForbiddenText);
  //   });
  //   it("Prevents a non-member user from querying an aggregation", async () => {
  //     const res: any = await lambdaPromise(
  //       graphql,
  //       genGraphQLProxy(aggregationsQuery, invalidUsername),
  //       {}
  //     );
  //     const body = JSON.parse(res.body);
  //     expect(body.errors[0].message).to.equal(expectedForbiddenText);
  //   });
  // });
  // describe("Transactions", function () {
  //   it("Get by Committee ID", async () => {
  //     const txn = genContributionRecord(committeeId);
  //     await putTransaction(txnsTableName)(dynamoDB)(txn);
  //     await sleep(1000);
  //     const res: any = await lambdaPromise(
  //       graphql,
  //       genGraphQLProxy(getAllTransactionsQuery, validUsername),
  //       {}
  //     );
  //     const body = JSON.parse(res.body);
  //     console.log(res.body);
  //     expect(body.data.transactions.length > 0).to.equal(true);
  //   });
  //   it("Get by Committee ID and Donor ID", async () => {
  //     const txn = genContributionRecord(committeeId);
  //     await putTransaction(txnsTableName)(dynamoDB)(txn);
  //     await sleep(1000);
  //
  //     const res: any = await lambdaPromise(
  //       graphql,
  //       genGraphQLProxy(
  //         getTransactionsByDonorIdQuery(txn.donorId),
  //         validUsername
  //       ),
  //       {}
  //     );
  //     const body: any = JSON.parse(res.body);
  //     expect(body.data.transactions[0].donorId).to.equal(txn.donorId);
  //   });
  // });
  // describe("Committee", function () {
  //   it("Get by Committee ID", async () => {
  //     const res: any = await lambdaPromise(
  //       graphql,
  //       genGraphQLProxy(getCommitteeQuery, validUsername),
  //       {}
  //     );
  //     const body = JSON.parse(res.body);
  //     expect(body.data.committee.id).to.equal(committee.id);
  //   });
  // });
  // describe("Aggregations", function () {
  //   it("Get by Committee ID", async () => {
  //     const query = genGraphQLProxy(aggregationsQuery, validUsername);
  //     console.log(query);
  //     const res: any = await lambdaPromise(graphql, query, {});
  //
  //     const body = JSON.parse(res.body);
  //     expect(body.data.aggregations.balance).to.be.a("number");
  //   });
  // });
  //
  // describe("Create Contributions", function () {
  //   it("Supports the creation of a contribution", async () => {
  //     const res: any = await lambdaPromise(
  //       graphql,
  //       genGraphQLProxy(
  //         createContributionQuery,
  //         validUsername,
  //         createContributionVariables
  //       ),
  //       {}
  //     );
  //
  //     const body = JSON.parse(res.body);
  //     console.log(res.body);
  //     expect(body.data.createContribution.amount).to.equal(12000);
  //   });
  // });
  // describe("Create Disbursement", function () {
  //   it("Supports the creation of a disbursement", async () => {
  //     const inputVar = genCreateDisbInput({
  //       committeeId,
  //     });
  //
  //     const createRes: any = await lambdaPromise(
  //       graphql,
  //       genGraphQLProxy(createDisb, validUsername, inputVar),
  //       {}
  //     );
  //
  //     const body = JSON.parse(createRes.body);
  //
  //     const tid = body.data.createDisbursement.id;
  //
  //     const txnRes: any = await lambdaPromise(
  //       graphql,
  //       genGraphQLProxy(getTxnQuery(committee.id)(tid), validUsername, {
  //
  //       }),
  //       {}
  //     );
  //
  //     const txnResBody = JSON.parse(txnRes.body);
  //
  //     expect(txnResBody.data.transaction.id).to.equal(tid);
  //   });
  //   it("Rejects a check disbursement missing a check number", async () => {
  //     const inputVar = genCreateDisbInput({
  //       committeeId,
  //       paymentMethod: PaymentMethod.Check,
  //     });
  //
  //     const createRes: any = await lambdaPromise(
  //       graphql,
  //       genGraphQLProxy(createDisb, validUsername, inputVar),
  //       {}
  //     );
  //     console.log(createRes);
  //
  //     const resBody = JSON.parse(createRes.body);
  //
  //     expect(resBody.errors.length > 0).to.equal(true);
  //   });
  // });
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
  // describe("Transaction", function () {
  //   it("Gets a transaction by id and committeeId", async () => {
  //     const createRes: any = await lambdaPromise(
  //       graphql,
  //       genGraphQLProxy(
  //         createContributionQuery,
  //         validUsername,
  //         createContributionVariables
  //       ),
  //       {}
  //     );
  //
  //     const body = JSON.parse(createRes.body);
  //
  //     const tid = body.data.createContribution.id;
  //
  //     const txnRes: any = await lambdaPromise(
  //       graphql,
  //       genGraphQLProxy(
  //         getTxnQuery(committee.id)(tid),
  //         validUsername,
  //         createContributionVariables
  //       ),
  //       {}
  //     );
  //
  //     const txnResBody = JSON.parse(txnRes.body);
  //
  //     expect(txnResBody.data.transaction.id).to.equal(tid);
  //   });
  //   it("Gets a 404 on bad committeeId", async () => {
  //     const tid = genTxnId();
  //
  //     const txnRes: any = await lambdaPromise(
  //       graphql,
  //       genGraphQLProxy(
  //         getTxnQuery(committee.id)(tid),
  //         validUsername,
  //         createContributionVariables
  //       ),
  //       {}
  //     );
  //
  //     const txnResBody = JSON.parse(txnRes.body);
  //     expect(txnResBody.data.transaction).to.equal(null);
  //   });
  // });
  // describe("Reconcile Disbursement", function () {
  //   it("Reconciles a disbursement by transaction id and a list of transaction ids", async () => {
  //     const createRes: any = await lambdaPromise(
  //       graphql,
  //       genGraphQLProxy(reconcileDisbMutation, validUsername, generalVariables),
  //       {}
  //     );
  //
  //     const body = JSON.parse(createRes.body);
  //
  //     console.log(createRes.body);
  //
  //     // const txnRes: any = await lambdaPromise(
  //     //   graphql,
  //     //   genGraphQLProxy(
  //     //     getTxnQuery(committee.id)(tid),
  //     //     validUsername,
  //     //     createContributionVariables
  //     //   ),
  //     //   {}
  //     // );
  //
  //     // const txnResBody = JSON.parse(txnRes.body);
  //
  //     // expect(body.data.transaction.id).to.equal(tid);
  //   });
  // });

  after(async () => {
    await deleteCommittee(committeesTableName)(dynamoDB)(committee);
  });
});
