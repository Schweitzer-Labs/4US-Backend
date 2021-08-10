export const verifiedDispatch = (committeeId: string) => (txnId: string) => ({
  "Records": [
    {
      "eventID": "c38b20ed603450bb420b94cdfe23a9d9",
      "eventName": "MODIFY",
      "eventVersion": "1.1",
      "eventSource": "aws:dynamodb",
      "awsRegion": "us-west-2",
      "dynamodb": {
        "ApproximateCreationDateTime": 1628608373,
        "Keys": {
          "committeeId": {
            "S": committeeId
          },
          "id": {
            "S": txnId
          }
        },
        "NewImage": {
          "lastName": {
            "S": "Rodriguez"
          },
          "finicityDescription": {
            "S": "ORIG CO NAME:Policapital"
          },
          "occupation": {
            "S": "Principia Maths"
          },
          "city": {
            "S": "Ridgewood"
          },
          "committeeId": {
            "S": committeeId
          },
          "postalCode": {
            "S": "11385"
          },
          "finicityTransactionId": {
            "N": "10320507772"
          },
          "source": {
            "S": "dashboard"
          },
          "employmentStatus": {
            "S": "Employed"
          },
          "finicityBestRepresentation": {
            "S": "ORIG CO NAME POLICAPITAL ORIG ID DESC DATE CO ENTRY DESCR POLICAPITASEC CCD TRACE EED IND ID ST IND NAME SCHWEITZER LABORATORIE TRN TC"
          },
          "stripeTransferId": {
            "S": "tr_1J3iZwEUhH8cxK5gZIm0uvLI"
          },
          "finicityTransactionData": {
            "M": {
              "accountId": {
                "N": "5016000964"
              },
              "amount": {
                "N": "2.5"
              },
              "createdDate": {
                "N": "1624460812"
              },
              "categorization": {
                "M": {
                  "country": {
                    "S": "USA"
                  },
                  "bestRepresentation": {
                    "S": "ORIG CO NAME POLICAPITAL ORIG ID DESC DATE CO ENTRY DESCR POLICAPITASEC CCD TRACE EED IND ID ST IND NAME SCHWEITZER LABORATORIE TRN TC"
                  },
                  "category": {
                    "S": "Income"
                  },
                  "normalizedPayeeName": {
                    "S": "Desc"
                  }
                }
              },
              "customerId": {
                "N": "5007489410"
              },
              "description": {
                "S": "ORIG CO NAME:Policapital"
              },
              "memo": {
                "S": "ORIG ID:4270465600 DESC DATE:       CO ENTRY DESCR:PolicapitaSEC:CCD    TRACE#:111000026506921 EED:210622   IND ID:ST-G0G9U4P7D3T0              IND NAME:SCHWEITZER LABORATORIE TRN: 1736506921TC"
              },
              "id": {
                "N": "10320507772"
              },
              "type": {
                "S": "credit"
              },
              "transactionDate": {
                "N": "1624359600"
              },
              "status": {
                "S": "active"
              },
              "postedDate": {
                "N": "1624359600"
              }
            }
          },
          "emailAddress": {
            "S": "dev.evanpiro@gmail.com"
          },
          "stripeAutomaticPayoutEffectiveAtUtc": {
            "N": "1624334400000"
          },
          "addressLine1": {
            "S": "1687 Gates Ave 1R"
          },
          "employer": {
            "S": "Software Engineer"
          },
          "id": {
            "S": txnId
          },
          "state": {
            "S": "ny"
          },
          "stripePaymentIntentId": {
            "S": "pi_1J3iZuEUhH8cxK5g6qAJTpo7"
          },
          "cardNumberLastFourDigits": {
            "S": "4465"
          },
          "direction": {
            "S": "In"
          },
          "amount": {
            "N": "50"
          },
          "stripeChargeId": {
            "S": "ch_1J3iZuEUhH8cxK5g4Vk1RmbY"
          },
          "finicityCategory": {
            "S": "Income"
          },
          "stripePaymentId": {
            "S": "py_1J3iZwRNcAftf9zRZ1lWXK2S"
          },
          "entityType": {
            "S": "Ind"
          },
          "bankVerified": {
            "BOOL": true
          },
          "attestsToBeingAnAdultCitizen": {
            "BOOL": true
          },
          "finicityTransactionDate": {
            "N": "1624359600000"
          },
          "finicityPostedDate": {
            "N": "1624359600000"
          },
          "transactionType": {
            "S": "Contribution"
          },
          "firstName": {
            "S": "Roger"
          },
          "ruleVerified": {
            "BOOL": true
          },
          "paymentMethod": {
            "S": "Credit"
          },
          "donorId": {
            "S": "1624026109465-XILLbb"
          },
          "paymentDate": {
            "N": "1624026113230"
          },
          "stripeBalanceTransactionId": {
            "S": "txn_1J3iZwEUhH8cxK5gTATwpucD"
          },
          "stripePayoutId": {
            "S": "po_1J4blORNcAftf9zR3TipdrAv"
          },
          "finicityNormalizedPayeeName": {
            "S": "Desc"
          },
          "initiatedTimestamp": {
            "N": "1624026113230"
          }
        },
        "OldImage": {
          "lastName": {
            "S": "Rodriguez"
          },
          "occupation": {
            "S": "Principia Maths"
          },
          "city": {
            "S": "Ridgewood"
          },
          "committeeId": {
            "S": committeeId
          },
          "postalCode": {
            "S": "11385"
          },
          "source": {
            "S": "dashboard"
          },
          "employmentStatus": {
            "S": "Employed"
          },
          "stripeTransferId": {
            "S": "tr_1J3iZwEUhH8cxK5gZIm0uvLI"
          },
          "emailAddress": {
            "S": "dev.evanpiro@gmail.com"
          },
          "stripeAutomaticPayoutEffectiveAtUtc": {
            "N": "1624334400000"
          },
          "addressLine1": {
            "S": "1687 Gates Ave 1R"
          },
          "employer": {
            "S": "Software Engineer"
          },
          "id": {
            "S": txnId
          },
          "state": {
            "S": "ny"
          },
          "stripePaymentIntentId": {
            "S": "pi_1J3iZuEUhH8cxK5g6qAJTpo7"
          },
          "cardNumberLastFourDigits": {
            "S": "4465"
          },
          "direction": {
            "S": "In"
          },
          "amount": {
            "N": "50"
          },
          "stripeChargeId": {
            "S": "ch_1J3iZuEUhH8cxK5g4Vk1RmbY"
          },
          "stripePaymentId": {
            "S": "py_1J3iZwRNcAftf9zRZ1lWXK2S"
          },
          "entityType": {
            "S": "Ind"
          },
          "bankVerified": {
            "BOOL": false
          },
          "attestsToBeingAnAdultCitizen": {
            "BOOL": true
          },
          "transactionType": {
            "S": "Contribution"
          },
          "firstName": {
            "S": "Roger"
          },
          "ruleVerified": {
            "BOOL": true
          },
          "paymentMethod": {
            "S": "Credit"
          },
          "donorId": {
            "S": "1624026109465-XILLbb"
          },
          "paymentDate": {
            "N": "1624026113230"
          },
          "stripeBalanceTransactionId": {
            "S": "txn_1J3iZwEUhH8cxK5gTATwpucD"
          },
          "stripePayoutId": {
            "S": "po_1J4blORNcAftf9zR3TipdrAv"
          },
          "initiatedTimestamp": {
            "N": "1624026113230"
          }
        },
        "SequenceNumber": "309025000000000013201866217",
        "SizeBytes": 2631,
        "StreamViewType": "NEW_AND_OLD_IMAGES"
      },
      "eventSourceARN": "arn:aws:dynamodb:us-west-2:396408709989:table/qa-4us-backend-Transactions-1JHQO9WAOZDJD/stream/2021-06-07T07:09:17.464"
    }
  ]
})
