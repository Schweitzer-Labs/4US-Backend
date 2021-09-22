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
import { genCreateContribInput } from "../utils/gen-create-contrib-input.util";
import { lambdaPromise } from "../../src/utils/lambda-promise.util";
import { CreateContributionInput } from "../../src/input-types/create-contribution.input-type";
import { State } from "../../src/utils/enums/state.enum";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;

const validUsername = qaUsers[0];

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

describe("GraphQL Lambda with Owners", function () {
  before(async () => {
    await putCommittee(committeesTableName)(dynamoDB)(committee);
    await sleep(1000);
  });
  describe("Create Contributions", function () {
    it("Supports the creation of a contribution with owners", async () => {
      const vars: CreateContributionInput = {
        ...genCreateContribInput({ committeeId }),
        owners: [
          {
            firstName: "rick",
            lastName: "smith",
            addressLine1: "123 place",
            addressLine2: "2r",
            city: "philadelphia",
            state: State.PA,
            postalCode: "11324",
            percentOwnership: "25",
          },
        ],
      };

      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(createContribMut, validUsername, vars),
        {}
      );

      const body = JSON.parse(res.body);
      expect(body.data.createContribution.amount).to.equal(vars.amount);
    });
    it("Rejects a contribution with bad owner breakdown", async () => {
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
  after(async () => {
    await deleteCommittee(committeesTableName)(dynamoDB)(committee);
  });
});
