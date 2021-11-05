import { expect } from "chai";
import graphql from "../../src/committee-graphql.lambda";
import { genGraphQLProxy } from "../utils/gen-allowed-proxy.util";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { genCommittee } from "../utils/gen-committee.util";
import { putCommittee } from "../../src/utils/model/committee/put-committee.utils";
import * as dotenv from "dotenv";
import { qaUsers } from "../seed/qa-users.data";
import { deleteCommittee } from "../../src/utils/model/committee/delete-committee.utils";
import { genCreateContribInput } from "../utils/gen-create-contrib-input.util";
import { lambdaPromise } from "../../src/utils/lambda-promise.util";
import { CreateContributionInput } from "../../src/graphql/input-types/create-contribution.input-type";
import { State } from "../../src/utils/enums/state.enum";
import { EntityType } from "../../src/utils/enums/entity-type.enum";

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
      $owners: [Owner!]
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
        owners: $owners
      }) {
        id
        amount
      }
    }
`;

describe("GraphQL Lambda with Owners", function () {
  before(async () => {
    await putCommittee(committeesTableName)(dynamoDB)(committee);
  });
  describe("Create Contributions", function () {
    it("Supports the creation of a contribution with owners", async () => {
      const vars: CreateContributionInput = {
        ...genCreateContribInput({ committeeId }),
        entityType: EntityType.Llc,
        amount: 500000,
        owners: [
          {
            firstName: "rick",
            lastName: "smith",
            addressLine1: "123 place",
            addressLine2: "2r",
            city: "philadelphia",
            state: State.PA,
            postalCode: "11324",
            percentOwnership: "100",
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
      const vars: CreateContributionInput = {
        ...genCreateContribInput({ committeeId }),
        entityType: EntityType.Llc,
        amount: 500000,
        owners: [
          {
            firstName: "rick",
            lastName: "smith",
            addressLine1: "123 place",
            addressLine2: "2r",
            city: "philadelphia",
            state: State.PA,
            postalCode: "11324",
            percentOwnership: "90",
          },
        ],
      };

      const res: any = await lambdaPromise(
        graphql,
        genGraphQLProxy(createContribMut, validUsername, vars),
        {}
      );

      const body = JSON.parse(res.body);

      expect(body.errors.length > 0).to.equal(true);
    });
  });
  after(async () => {
    await deleteCommittee(committeesTableName)(dynamoDB)(committee);
  });
});
