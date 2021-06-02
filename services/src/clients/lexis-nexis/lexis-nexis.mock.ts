export const mockRes = {
  InstantIDResponseEx: {
    "@xmlns": "http://webservices.seisint.com/WsIdentity",
    response: {
      Header: {
        Status: 0,
        TransactionId: "71038987R151737",
      },
      Result: {
        InputEcho: {
          Name: {
            First: "Evan",
            Last: "Piro",
          },
          Address: {
            StreetAddress1: "448 Stockholm Street",
            StreetAddress2: "2FL",
            City: "Ridgewood",
            State: "NY",
            Zip5: "11385",
          },
          Email: "dev.evanpiro@gmail.com",
        },
        UniqueId: "145643347251",
        VerifiedInput: {
          Name: {
            First: "EVAN",
            Last: "PIRO",
          },
          Address: {
            StreetNumber: "448",
            StreetName: "STOCKHOLM",
            StreetSuffix: "ST",
            UnitNumber: "2FL",
            StreetAddress1: "448 STOCKHOLM ST 2FL",
            City: "RIDGEWOOD",
            State: "NY",
            Zip5: "11385",
            County: "QUEENS",
          },
        },
        DOBVerified: false,
        NameAddressSSNSummary: 2,
        NameAddressPhone: {
          Summary: "8",
          Type: "S",
        },
        ComprehensiveVerification: {
          ComprehensiveVerificationIndex: 20,
          RiskIndicators: {
            RiskIndicator: [
              {
                RiskCode: "79",
                Description: "The input SSN/TIN was missing or incomplete",
                Sequence: 1,
              },
              {
                RiskCode: "SD",
                Description:
                  "The input address State is different than the LN best address State for the input identity",
                Sequence: 2,
              },
              {
                RiskCode: "81",
                Description:
                  "The input date-of-birth was missing or incomplete",
                Sequence: 3,
              },
              {
                RiskCode: "80",
                Description: "The input phone was missing or incomplete",
                Sequence: 4,
              },
              {
                RiskCode: "EU",
                Description: "The input email address is not verified",
                Sequence: 5,
              },
            ],
          },
          PotentialFollowupActions: {
            FollowupAction: [
              {
                RiskCode: "B",
                Description:
                  "Verify name with Social (via SSN card, DL if applicable, paycheck stub, or other Government Issued ID)",
              },
              {
                RiskCode: "D",
                Description:
                  "Verify phone (Directory Assistance, utility bill)",
              },
            ],
          },
        },
        ChronologyHistories: {
          ChronologyHistory: [
            {
              Address: {
                StreetNumber: "1363",
                StreetName: "PENNSRIDGE",
                StreetSuffix: "PL",
                StreetAddress1: "1363 PENNSRIDGE PL",
                City: "DOWNINGTOWN",
                State: "PA",
                Zip5: "19335",
                Zip4: "3682",
              },
              Phone: "6108734152",
              DateFirstSeen: {
                Year: 2009,
                Month: 1,
              },
              DateLastSeen: {
                Year: 2010,
                Month: 1,
              },
              IsBestAddress: true,
            },
          ],
        },
        AdditionalScore1: "0",
        AdditionalScore2: "0",
        CurrentName: {
          First: "EVAN",
          Last: "PIRO",
        },
        PassportValidated: false,
        DOBMatchLevel: 0,
        SSNFoundForLexID: false,
        AddressPOBox: false,
        AddressCMRA: false,
        InstantIDVersion: "1",
        EmergingId: false,
        AddressStandardized: true,
        StandardizedInputAddress: {
          StreetNumber: "448",
          StreetName: "STOCKHOLM",
          StreetSuffix: "ST",
          UnitNumber: "2FL",
          StreetAddress1: "448 STOCKHOLM ST 2FL",
          StreetAddress2: "RIDGEWOOD, NY 11385",
          City: "RIDGEWOOD",
          State: "NY",
          Zip5: "11385",
          Zip4: "1333",
          County: "QUEENS",
          Latitude: "40.706440",
          Longitude: "-73.915680",
        },
        AddressSecondaryRangeMismatch: "V",
        BureauDeleted: false,
        ITINExpired: false,
        IsPhoneCurrent: false,
      },
    },
  },
};
