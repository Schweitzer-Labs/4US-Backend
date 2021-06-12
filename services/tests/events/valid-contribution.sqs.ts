export const validContributionSqs = {
  "Records": [
    {
      "messageId": "4e86fa50-208a-456c-b83d-6cd2d9461c7d",
      "receiptHandle": "AQEBsX4WrpgUBGtEcgQROh3xkTiArurdyrEBeBSEoHBBfL/lKSr8FThHcfjnZhq7YbpqA4yAnUnueQ3qWdv3gaD7G3C75+dlnev3liQgWF5PMYwwt7bHeDljbTskl6ICYCK+fnAjbAx1phDwAkxSIls1hnr8208P+kka0faJtrOS17fj0W8GxS9mQhvPa1/JreSJk2s0qtn4PS5kJb5FHVIDVDtvZHmO4IOuDe/vwgYBHyhWN9kSmnWRWiVSpslXLghehBKSGV7wb8WvNcu0kUDquEorm8HcbayLYnEoxdn9zZ9mswCE0NcOGhKpuvKvdgiO",
      "body": "{\"attestsToBeingAdultCitizen\":true,\"lastName\":\"Hubburd\",\"occupation\":\"\",\"city\":\"New Sadiechester\",\"committeeId\":\"angel-cruz\",\"companyName\":\"\",\"postalCode\":\"69405\",\"source\":\"donate_form\",\"addressLine1\":\"139 Adrianna Squares\",\"employer\":\"\",\"addressLine2\":\"Apt. 382\",\"id\":\"6ef02faa-1c69-4352-a656-c75e4ab14fe3\",\"state\":\"wi\",\"emailAddress\":\"dev.evanpiro@gmail.com\",\"stripePaymentIntentId\":\"pi_1IuVUEEUhH8cxK5gwTmhDCjY\",\"cardNumberLastFourDigits\":\"4242\",\"direction\":\"in\",\"amount\":10000,\"committee\":\"angel-cruz\",\"bankVerified\":false,\"stripeUserId\":\"acct_1IjTcsRC8iiQex3V\",\"transactionType\":\"contribution\",\"firstName\":\"Charles\",\"phoneNumber\":\"\",\"ruleVerified\":false,\"paymentMethod\":\"credit\",\"entityType\":\"ind\",\"refCode\":\"\",\"initiatedTimestamp\": 1621830835460 }",
      "attributes": {
        "ApproximateReceiveCount": "26",
        "SentTimestamp": "1621830836760",
        "SequenceNumber": "18861932767920112128",
        "MessageGroupId": "angel-cruz",
        "SenderId": "AROAVYS6M4NSWFWOL6BHN:purplepay-backend-dev-TransactionEventDispatcherFunction",
        "MessageDeduplicationId": "6ef02faa-1c69-4352-a656-c75e4ab14fe3",
        "ApproximateFirstReceiveTimestamp": "1621830836760"
      },
      "messageAttributes": {
        "committeeEmailAddress": {
          "stringValue": "evan@schweitzerlabs.com,seemant@schweitzerlabs.com",
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
          "stringValue": "Angel Cruz for Bronx Judge",
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
