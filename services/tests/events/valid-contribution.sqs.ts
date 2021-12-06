import * as faker from "faker";
import {genTxnId} from "../../src/utils/gen-txn-id.utils";
import {ITransaction} from "../../src/model/transaction.type";
import {now} from "../../src/utils/time.utils";
import {EntityType} from "../../src/utils/enums/entity-type.enum";
import {PaymentMethod} from "../../src/utils/enums/payment-method.enum";
import {State} from "../../src/utils/enums/state.enum";
import {Source} from "../../src/utils/enums/source.enum";
import {Direction} from "../../src/utils/enums/direction.enum";
import {TransactionType} from "../../src/utils/enums/transaction-type.enum";

const genBody = (): string => JSON.stringify(<ITransaction>{
  id: genTxnId(),
  attestsToBeingAdultCitizen: true,
  firstName: faker.name.firstName(),
  lastName: faker.name.lastName(),
  occupation: faker.name.jobTitle(),
  addressLine1: faker.address.streetAddress(),
  addressLine2: faker.address.secondaryAddress(),
  city: faker.address.city(),
  state: State.MA,
  employer: "",
  committee: genTxnId(),
  committeeId: genTxnId(),
  companyName: faker.company.companyName(),
  postalCode: faker.address.zipCode(),
  phoneNumber: "555-555-5555",
  paymentDate: now(),
  initiatedTimestamp: now(),
  entityType: EntityType.Ind,
  paymentMethod: PaymentMethod.Credit,
  cardNumberLastFourDigits: "4242",
  stripePaymentIntentId: genTxnId(),
  emailAddress: "evan@schweitzerlabs.com",
  source: Source.DONATE_FORM,
  bankVerified: false,
  ruleVerified: true,
  direction: Direction.In,
  amount: faker.datatype.number(4950) + 50,
  refCode: 'test',
  transactionType: TransactionType.Contribution,
})

export const genValidContributionSqs = () => ({
    "Records": [
      {
        "messageId": "4e86fa50-208a-456c-b83d-6cd2d9461c7d",
        "receiptHandle": "AQEBsX4WrpgUBGtEcgQROh3xkTiArurdyrEBeBSEoHBBfL/lKSr8FThHcfjnZhq7YbpqA4yAnUnueQ3qWdv3gaD7G3C75+dlnev3liQgWF5PMYwwt7bHeDljbTskl6ICYCK+fnAjbAx1phDwAkxSIls1hnr8208P+kka0faJtrOS17fj0W8GxS9mQhvPa1/JreSJk2s0qtn4PS5kJb5FHVIDVDtvZHmO4IOuDe/vwgYBHyhWN9kSmnWRWiVSpslXLghehBKSGV7wb8WvNcu0kUDquEorm8HcbayLYnEoxdn9zZ9mswCE0NcOGhKpuvKvdgiO",
        "body": genBody(),
        "attributes": {
          "ApproximateReceiveCount": "26",
          "SentTimestamp": "1621830836760",
          "SequenceNumber": "18861932767920112128",
          "MessageGroupId": genTxnId(),
          "SenderId": "AROAVYS6M4NSWFWOL6BHN:purplepay-backend-dev-TransactionEventDispatcherFunction",
          "MessageDeduplicationId": "6ef02faa-1c69-4352-a656-c75e4ab14fe3",
          "ApproximateFirstReceiveTimestamp": "1621830836760"
        },
        "messageAttributes": {
          "committeeEmailAddress": {
            "stringValue": "evan@schweitzerlabs.com",
            "stringListValues": [],
            "binaryListValues": [],
            "dataType": "String"
          },
          "committeeTzDatabaseName": {
            "stringValue": "America/New_York",
            "stringListValues": [],
            "binaryListValues": [],
            "dataType": "String"
          },
          "committeeName": {
            "stringValue": `${faker.name.findName()} for State Senate`,
            "stringListValues": [],
            "binaryListValues": [],
            "dataType": "String"
          }
        },
        "md5OfMessageAttributes": "ba2559c4d8312d8d578551fb33d2aa3a",
        "md5OfBody": "3139b55a34a814a30f6b94f22ef40389",
        "eventSource": "aws:sqs",
        "eventSourceARN": "arn:aws:sqs:us-east-1:396408709989:dev-purplepay-ReceiptEmailQueue.fifo",
        "awsRegion": "us-east-1"
      }
    ]
  }
)