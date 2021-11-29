export const getAllTransactionsQuery = (committeeId: string) => `
  query {
    transactions(committeeId: "${committeeId}") {
      lastName
      firstName
      amount
      direction
    }
  }
`;

export const getTransactionsByDonorIdQuery =
  (committeeId: string) => (donorId: string) =>
    `
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

export const getTxnQuery = (committeeId) => (tid: string) =>
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
      businessIdVerificationScore
    }
  }
`;

export const getCommitteeQuery = (committeeId: string) => `
  query {
    committee(committeeId: "${committeeId}") {
      id
      candidateFirstName
    }
  }
`;

export const aggregationsQuery = (committeeId: string) => `
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

export const createDisb = `
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

export const amendDisbMut = `
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

export const createContribMut = `
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

export const amendContribMut = `
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

export const recTxnMutation = `
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

export const deleteTxnMut = `
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
