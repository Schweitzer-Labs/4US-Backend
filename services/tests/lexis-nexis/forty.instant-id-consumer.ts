export const fortyInstantIdConsumer = {
  InstantIDResponseEx: {
    "@xmlns": "http://webservices.seisint.com/WsIdentity",
    response: {
      Header: {
        Status: 0,
        TransactionId: "103889400R993136",
      },
      Result: {
        InputEcho: {
          Name: {
            First: "Lily",
            Last: "Martin",
          },
          Address: {
            StreetAddress1: "448 Stockholm Street",
            StreetAddress2: "2FL",
            City: "Queens",
            State: "NY",
            Zip5: "11385",
          },
          HomePhone: "3474544110",
          Email: "lilykcmartin@gmail.com",
        },
        UniqueId: "148930672239",
        VerifiedInput: {
          Name: {
            First: "LILY",
            Last: "MARTIN",
          },
          Address: {
            StreetNumber: "448",
            StreetName: "STOCKHOLM",
            StreetSuffix: "ST",
            StreetAddress1: "448 STOCKHOLM ST",
            City: "RIDGEWOOD",
            State: "NY",
            Zip5: "11385",
            Zip4: "1333",
            County: "QUEENS",
          },
          HomePhone: "3474544110",
        },
        DOBVerified: false,
        NameAddressSSNSummary: 8,
        NameAddressPhone: {
          Summary: "12",
          Type: "P",
          Status: "C",
        },
        ComprehensiveVerification: {
          ComprehensiveVerificationIndex: 40,
          RiskIndicators: {
            RiskIndicator: [
              {
                RiskCode: "79",
                Description: "The input SSN/TIN was missing or incomplete",
                Sequence: 1,
              },
              {
                RiskCode: "10",
                Description: "The input phone number is a mobile number",
                Sequence: 2,
              },
              {
                RiskCode: "82",
                Description:
                  "The input name and address return a different phone number",
                Sequence: 3,
              },
              {
                RiskCode: "64",
                Description:
                  "The input address returns a different phone number",
                Sequence: 4,
              },
              {
                RiskCode: "81",
                Description:
                  "The input date-of-birth was missing or incomplete",
                Sequence: 5,
              },
              {
                RiskCode: "EU",
                Description: "The input email address is not verified",
                Sequence: 6,
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
                RiskCode: "C",
                Description:
                  "Verify name with Address (via DL, utility bill, Directory Assistance, paycheck stub, or other Government Issued ID)",
              },
            ],
          },
        },
        ReversePhone: {
          Name: {
            First: "LILY",
            Last: "MARTIN",
          },
          Address: {
            StreetNumber: "448",
            StreetName: "STOCKHOLM",
            StreetSuffix: "ST",
            StreetAddress1: "448 STOCKHOLM ST",
            City: "RIDGEWOOD",
            State: "NY",
            Zip5: "11385",
          },
        },
        PhoneOfNameAddress: "7188211636",
        ChronologyHistories: {
          ChronologyHistory: [
            {
              Address: {
                StreetNumber: "448",
                StreetName: "STOCKHOLM",
                StreetSuffix: "ST",
                StreetAddress1: "448 STOCKHOLM ST",
                City: "RIDGEWOOD",
                State: "NY",
                Zip5: "11385",
                Zip4: "1333",
              },
              DateFirstSeen: {
                Year: 2020,
                Month: 9,
              },
              DateLastSeen: {
                Year: 2020,
                Month: 9,
              },
              IsBestAddress: true,
            },
            {
              Address: {
                StreetNumber: "3705",
                StreetName: "30TH",
                StreetSuffix: "AVE",
                UnitDesignation: "APT",
                UnitNumber: "4",
                StreetAddress1: "3705 30TH AVE APT 4",
                City: "ASTORIA",
                State: "NY",
                Zip5: "11103",
                Zip4: "4338",
              },
              DateFirstSeen: {
                Year: 2020,
                Month: 4,
              },
              DateLastSeen: {
                Year: 2020,
                Month: 4,
              },
            },
            {
              Address: {
                StreetNumber: "943",
                StreetName: "NORTH",
                StreetSuffix: "RD",
                StreetAddress1: "943 NORTH RD",
                City: "NORTH YARMOUTH",
                State: "ME",
                Zip5: "04097",
                Zip4: "6933",
              },
              Phone: "2078384500",
            },
          ],
        },
        AdditionalScore1: "0",
        AdditionalScore2: "0",
        CurrentName: {
          First: "LILY",
          Last: "MARTIN",
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
        AddressSecondaryRangeMismatch: "I",
        BureauDeleted: false,
        ITINExpired: false,
        IsPhoneCurrent: true,
        PhoneLineType: "1",
        PhoneLineDescription: "W",
      },
    },
  },
};
