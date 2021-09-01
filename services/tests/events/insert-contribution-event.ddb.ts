export const insertContributionEvent = (committeeId: string) => ({
  "Records": [
    {
      "eventID": "9e5cb72b65445a16093ca586467e3794",
      "eventName": "INSERT",
      "eventVersion": "1.1",
      "eventSource": "aws:dynamodb",
      "awsRegion": "us-east-1",
      "dynamodb": {
        "ApproximateCreationDateTime": 1621795836,
        "Keys": {
          "committeeId": {
            "S": committeeId
          },
          "id": {
            "S": "1621795836019-zjnlfa"
          }
        },
        "NewImage": {
          "lastName": {
            "S": "Johnson"
          },
          "amount": {
            "N": "2000"
          },
          "city": {
            "S": "Woodside"
          },
          "entityType": {
            "S": "ind"
          },
          "committeeId": {
            "S": committeeId
          },
          "postalCode": {
            "S": "11377"
          },
          "source": {
            "S": "donate_form"
          },
          "bankVerified": {
            "BOOL": false
          },
          "firstName": {
            "S": "Chris"
          },
          "ruleVerified": {
            "BOOL": false
          },
          "addressLine1": {
            "S": "1429 Buckingham Road"
          },
          "employer": {
            "S": "Google"
          },
          "paymentMethod": {
            "S": "credit"
          },
          "id": {
            "S": "1621795836019-zjnlfa"
          },
          "state": {
            "S": "NY"
          },
          "refCode": {
            "S": "home-page"
          },
          "initiatedTimestamp": {
            "N": 1621795836019
          },
          "paymentDate": {
            "N": 1621795836019
          },
          "cardNumberLastFourDigits": {
            "S": "1232"
          },
          "emailAddress": {
            "S": "evan@schweitzerlabs.com"
          },
          "direction": {
            "S": "in"
          },
          "tzDatabaseName": {
            "S": "America/New_York"
          }
        },
        "SequenceNumber": "23283700000000006358742399",
        "SizeBytes": 399,
        "StreamViewType": "NEW_AND_OLD_IMAGES"
      },
      "eventSourceARN": "arn:aws:dynamodb:us-east-1:396408709989:table/transactions-dev/stream/2021-05-23T17:53:12.489"
    }
  ]
})

