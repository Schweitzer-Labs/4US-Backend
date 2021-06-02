export const zeroInstantIdConsumer = {
  InstantIDResponseEx: {
    "@xmlns": "http://webservices.seisint.com/WsIdentity",
    response: {
      Header: {
        Status: 0,
        TransactionId: "103890880R960305",
      },
      Result: {
        InputEcho: {
          Name: {
            First: "Christopher",
            Last: "Perry",
          },
          Address: {
            StreetAddress1: "24 Melrose Street",
            StreetAddress2: "2FL",
            City: "Queens",
            State: "NY",
            Zip5: "11237",
          },
          HomePhone: "6103452839",
          Email: "chrisperry@gmail.com",
        },
        UniqueId: "0",
        DOBVerified: false,
        NameAddressSSNSummary: 0,
        NameAddressPhone: {
          Summary: "0",
        },
        ComprehensiveVerification: {
          ComprehensiveVerificationIndex: 0,
          RiskIndicators: {
            RiskIndicator: [
              {
                RiskCode: "19",
                Description:
                  "Unable to verify name, address, SSN/TIN and phone",
                Sequence: 1,
              },
              {
                RiskCode: "79",
                Description: "The input SSN/TIN was missing or incomplete",
                Sequence: 2,
              },
              {
                RiskCode: "CZ",
                Description: "Address mismatch between city/state and zip code",
                Sequence: 3,
              },
              {
                RiskCode: "16",
                Description:
                  "The input phone number and input zip code combination is invalid",
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
              {
                RiskCode: "D",
                Description:
                  "Verify phone (Directory Assistance, utility bill)",
              },
            ],
          },
        },
        AdditionalScore1: "0",
        AdditionalScore2: "0",
        PassportValidated: false,
        DOBMatchLevel: 0,
        SSNFoundForLexID: false,
        AddressPOBox: false,
        AddressCMRA: false,
        InstantIDVersion: "1",
        EmergingId: false,
        AddressStandardized: true,
        StandardizedInputAddress: {
          StreetNumber: "24",
          StreetName: "MELROSE",
          StreetSuffix: "LN",
          UnitNumber: "2FL",
          StreetAddress1: "24 MELROSE LN 2FL",
          StreetAddress2: "LITTLE NECK, NY 11363",
          City: "LITTLE NECK",
          State: "NY",
          Zip5: "11363",
          Zip4: "1221",
          County: "QUEENS",
          Latitude: "40.772020",
          Longitude: "-73.752470",
        },
        AddressSecondaryRangeMismatch: "I",
        BureauDeleted: false,
        ITINExpired: false,
        IsPhoneCurrent: true,
        PhoneLineType: "2",
        PhoneLineDescription: "V",
      },
    },
  },
};
