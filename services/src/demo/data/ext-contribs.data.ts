import * as faker from "faker";
import { ITransaction } from "../../model/transaction.type";
import { ExternalSource } from "../../utils/enums/source.enum";
import { flow } from "fp-ts/function";
import { SeedExtContribsInput } from "../../graphql/input-types/seed-ext-contribs.input-type";
import { State } from "../../utils/enums/state.enum";

export const getExtContribs = (input: SeedExtContribsInput): ITransaction[] =>
  actblueExternalContribs.reduce(
    (acc, val) => [
      ...acc,
      flow(
        setCommitteeId(input.committeeId),
        anonymizeTxn,
        setSource(input.externalSource)
      )(val.contribTransaction),
      flow(
        setCommitteeId(input.committeeId),
        setSource(input.externalSource),
        setVendor(input.externalSource)
      )(val.feeTransaction),
    ],
    []
  );

const setCommitteeId =
  (committeeId: string) =>
  (txn: ITransaction): ITransaction => ({
    ...txn,
    committeeId,
  });

const anonymizeTxn = (txn: ITransaction): ITransaction => {
  const firstName = faker.name.firstName();
  const lastName = faker.name.lastName();
  return {
    ...txn,
    firstName,
    lastName,
    addressLine1: faker.address.streetAddress(),
    addressLine2: faker.address.secondaryAddress(),
    city: faker.address.city(),
    emailAddress: `${firstName}_${lastName}@yopmail.com`,
    phoneNumber: faker.phone.phoneNumber(),
    employer: faker.company.companyName(),
  };
};

const setSource =
  (source: ExternalSource) =>
  (txn: ITransaction): ITransaction => ({
    ...txn,
    source,
  });

const setVendor =
  (source: ExternalSource) =>
  (txn: ITransaction): ITransaction => {
    switch (source) {
      case ExternalSource.ActBlue:
        return {
          ...txn,
        };
      case ExternalSource.WinRed:
        return {
          ...txn,
          entityName: "WinRed Technical Services LLC",
          addressLine1: "1776 Wilson blvd",
          addressLine2: "Suite 530",
          city: "Arlington",
          state: State.VA,
          postalCode: "22219",
        };
    }
  };

const actblueExternalContribs: any = [
  {
    result: "Created",
    externalContribution: {
      id: "AB198873743",
      recipientId: "77822",
      source: "ActBlue",
      paymentDate: 1623847421000,
      emailAddress: "mullen.brendan@gmail.com",
      amount: 5000,
      firstName: "Brendan",
      lastName: "Mullen",
      addressLine1: "40 Driftway Unit 15",
      addressLine2: "",
      city: "Scituate",
      state: "MA",
      country: "United States",
      postalCode: "02066",
      phoneNumber: "",
      payoutId: "1353485",
      payoutDate: 1624161600000,
      occupation: "Investor",
      employer: "Secha Capital",
      refCode: "",
      entityType: "Ind",
      paymentMethod: "Credit",
      checkNumber: "400326312",
      processorFeeData: {
        amount: 198,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1624161600000,
        paymentMethod: "Check",
        checkNumber: "400326312",
      },
    },
    feeTransaction: {
      id: "1638467975364-KIZZLR",
      source: "ActBlue",
      amount: 198,
      entityName: "ActBlue Technical Services",
      addressLine1: "366 Summer Street",
      city: "Somerville",
      state: "MA",
      postalCode: "02144-3132",
      country: "US",
      paymentDate: 1624161600000,
      paymentMethod: "Check",
      checkNumber: "400326312",
      committeeId: "rec-txns-1638467958340-rdck6I",
      direction: "Out",
      bankVerified: false,
      ruleVerified: true,
      initiatedTimestamp: 1623847421000,
      transactionType: "Disbursement",
      isSubcontracted: false,
      isPartialPayment: false,
      isExistingLiability: false,
      purposeCode: "FUNDR",
      feeForTxn: "1638467975088-u7MGsh",
      externalTransactionId: "AB198873743",
      externalTransactionPayoutId: "1353485",
    },
    contribTransaction: {
      id: "1638467975088-u7MGsh",
      createdByUser: "system",
      donorId: "1637334757414-rGvxRl",
      donorVerificationScore: 0,
      ruleCode: "[ny][state][democrat][primary][53][][senate][nyboe-2020][ind]",
      initiatedTimestamp: 1638467975088,
      direction: "In",
      transactionType: "Contribution",
      bankVerified: false,
      ruleVerified: true,
      source: "ActBlue",
      externalTransactionId: "AB198873743",
      externalTransactionPayoutId: "1353485",
      processPayment: false,
      committeeId: "rec-txns-1638467958340-rdck6I",
      paymentMethod: "Credit",
      paymentDate: 1623847421000,
      amount: 5000,
      firstName: "Brendan",
      lastName: "Mullen",
      addressLine1: "40 Driftway Unit 15",
      addressLine2: "",
      city: "Scituate",
      state: "MA",
      postalCode: "02066",
      entityType: "Ind",
      emailAddress: "mullen.brendan@gmail.com",
      employer: "Secha Capital",
      occupation: "Investor",
      phoneNumber: "",
      checkNumber: "400326312",
      processorFeeData: {
        amount: 198,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1624161600000,
        paymentMethod: "Check",
        checkNumber: "400326312",
      },
    },
  },
  {
    result: "Created",
    externalContribution: {
      id: "AB198885405",
      recipientId: "77822",
      source: "ActBlue",
      paymentDate: 1623769996000,
      emailAddress: "dunphy001@gmail.com",
      amount: 5000,
      firstName: "James",
      lastName: "Dunphy",
      addressLine1: "76 Harbourside Road",
      addressLine2: "",
      city: "Quincy",
      state: "MA",
      country: "United States",
      postalCode: "02171",
      phoneNumber: "7815345344",
      payoutId: "1353485",
      payoutDate: 1624161600000,
      occupation: "Banking",
      employer: "South Shore Bank",
      refCode: "",
      entityType: "Ind",
      paymentMethod: "Credit",
      checkNumber: "400326312",
      processorFeeData: {
        amount: 198,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1624161600000,
        paymentMethod: "Check",
        checkNumber: "400326312",
      },
    },
    feeTransaction: {
      id: "1638467976970-jKV2c6",
      source: "ActBlue",
      amount: 198,
      entityName: "ActBlue Technical Services",
      addressLine1: "366 Summer Street",
      city: "Somerville",
      state: "MA",
      postalCode: "02144-3132",
      country: "US",
      paymentDate: 1624161600000,
      paymentMethod: "Check",
      checkNumber: "400326312",
      committeeId: "rec-txns-1638467958340-rdck6I",
      direction: "Out",
      bankVerified: false,
      ruleVerified: true,
      initiatedTimestamp: 1623769996000,
      transactionType: "Disbursement",
      isSubcontracted: false,
      isPartialPayment: false,
      isExistingLiability: false,
      purposeCode: "FUNDR",
      feeForTxn: "1638467976705-S4X6mL",
      externalTransactionId: "AB198885405",
      externalTransactionPayoutId: "1353485",
    },
    contribTransaction: {
      id: "1638467976705-S4X6mL",
      createdByUser: "system",
      donorId: "1637334761486-FVV6gU",
      donorVerificationScore: 0,
      ruleCode: "[ny][state][democrat][primary][53][][senate][nyboe-2020][ind]",
      initiatedTimestamp: 1638467976705,
      direction: "In",
      transactionType: "Contribution",
      bankVerified: false,
      ruleVerified: true,
      source: "ActBlue",
      externalTransactionId: "AB198885405",
      externalTransactionPayoutId: "1353485",
      processPayment: false,
      committeeId: "rec-txns-1638467958340-rdck6I",
      paymentMethod: "Credit",
      paymentDate: 1623769996000,
      amount: 5000,
      firstName: "James",
      lastName: "Dunphy",
      addressLine1: "76 Harbourside Road",
      addressLine2: "",
      city: "Quincy",
      state: "MA",
      postalCode: "02171",
      entityType: "Ind",
      emailAddress: "dunphy001@gmail.com",
      employer: "South Shore Bank",
      occupation: "Banking",
      phoneNumber: "7815345344",
      checkNumber: "400326312",
      processorFeeData: {
        amount: 198,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1624161600000,
        paymentMethod: "Check",
        checkNumber: "400326312",
      },
    },
  },
  {
    result: "Created",
    externalContribution: {
      id: "AB199094461",
      recipientId: "77822",
      source: "ActBlue",
      paymentDate: 1624037270000,
      emailAddress: "djm@atlanticdevelopment.com",
      amount: 25000,
      firstName: "D.J.",
      lastName: "MacKinnon",
      addressLine1: "P.O. Box 152",
      addressLine2: "",
      city: "Hingham",
      state: "MA",
      country: "United States",
      postalCode: "02043",
      phoneNumber: "6174295025",
      payoutId: "1353485",
      payoutDate: 1624161600000,
      occupation: "R.E. Developer",
      employer: "Atlantic Development",
      refCode: "",
      entityType: "Ind",
      paymentMethod: "Credit",
      checkNumber: "400326312",
      processorFeeData: {
        amount: 988,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1624161600000,
        paymentMethod: "Check",
        checkNumber: "400326312",
      },
    },
    feeTransaction: {
      id: "1638467978559-fickZU",
      source: "ActBlue",
      amount: 988,
      entityName: "ActBlue Technical Services",
      addressLine1: "366 Summer Street",
      city: "Somerville",
      state: "MA",
      postalCode: "02144-3132",
      country: "US",
      paymentDate: 1624161600000,
      paymentMethod: "Check",
      checkNumber: "400326312",
      committeeId: "rec-txns-1638467958340-rdck6I",
      direction: "Out",
      bankVerified: false,
      ruleVerified: true,
      initiatedTimestamp: 1624037270000,
      transactionType: "Disbursement",
      isSubcontracted: false,
      isPartialPayment: false,
      isExistingLiability: false,
      purposeCode: "FUNDR",
      feeForTxn: "1638467978293-6DwHqq",
      externalTransactionId: "AB199094461",
      externalTransactionPayoutId: "1353485",
    },
    contribTransaction: {
      id: "1638467978293-6DwHqq",
      createdByUser: "system",
      donorId: "1637334765951-pthrWu",
      donorVerificationScore: 0,
      ruleCode: "[ny][state][democrat][primary][53][][senate][nyboe-2020][ind]",
      initiatedTimestamp: 1638467978293,
      direction: "In",
      transactionType: "Contribution",
      bankVerified: false,
      ruleVerified: true,
      source: "ActBlue",
      externalTransactionId: "AB199094461",
      externalTransactionPayoutId: "1353485",
      processPayment: false,
      committeeId: "rec-txns-1638467958340-rdck6I",
      paymentMethod: "Credit",
      paymentDate: 1624037270000,
      amount: 25000,
      firstName: "D.J.",
      lastName: "MacKinnon",
      addressLine1: "P.O. Box 152",
      addressLine2: "",
      city: "Hingham",
      state: "MA",
      postalCode: "02043",
      entityType: "Ind",
      emailAddress: "djm@atlanticdevelopment.com",
      employer: "Atlantic Development",
      occupation: "R.E. Developer",
      phoneNumber: "6174295025",
      checkNumber: "400326312",
      processorFeeData: {
        amount: 988,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1624161600000,
        paymentMethod: "Check",
        checkNumber: "400326312",
      },
    },
  },
  {
    result: "Created",
    externalContribution: {
      id: "AB199236098",
      recipientId: "77822",
      source: "ActBlue",
      paymentDate: 1624291792000,
      emailAddress: "josephfbrophy@comcast.net",
      amount: 5000,
      firstName: "Joseph",
      lastName: "Brophy",
      addressLine1: "108 Granger St",
      addressLine2: "",
      city: "Quincy",
      state: "MA",
      country: "United States",
      postalCode: "02170",
      phoneNumber: "6174715353",
      payoutId: "1357955",
      payoutDate: 1624766400000,
      occupation: "Not Employed",
      employer: "Not Employed",
      refCode: "",
      entityType: "Ind",
      paymentMethod: "Credit",
      checkNumber: "400330883",
      processorFeeData: {
        amount: 198,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1624766400000,
        paymentMethod: "Check",
        checkNumber: "400330883",
      },
    },
    feeTransaction: {
      id: "1638467980173-ZkpQDv",
      source: "ActBlue",
      amount: 198,
      entityName: "ActBlue Technical Services",
      addressLine1: "366 Summer Street",
      city: "Somerville",
      state: "MA",
      postalCode: "02144-3132",
      country: "US",
      paymentDate: 1624766400000,
      paymentMethod: "Check",
      checkNumber: "400330883",
      committeeId: "rec-txns-1638467958340-rdck6I",
      direction: "Out",
      bankVerified: false,
      ruleVerified: true,
      initiatedTimestamp: 1624291792000,
      transactionType: "Disbursement",
      isSubcontracted: false,
      isPartialPayment: false,
      isExistingLiability: false,
      purposeCode: "FUNDR",
      feeForTxn: "1638467979903-gT3alL",
      externalTransactionId: "AB199236098",
      externalTransactionPayoutId: "1357955",
    },
    contribTransaction: {
      id: "1638467979903-gT3alL",
      createdByUser: "system",
      donorId: "1637334769090-IbAjhF",
      donorVerificationScore: 0,
      ruleCode: "[ny][state][democrat][primary][53][][senate][nyboe-2020][ind]",
      initiatedTimestamp: 1638467979903,
      direction: "In",
      transactionType: "Contribution",
      bankVerified: false,
      ruleVerified: true,
      source: "ActBlue",
      externalTransactionId: "AB199236098",
      externalTransactionPayoutId: "1357955",
      processPayment: false,
      committeeId: "rec-txns-1638467958340-rdck6I",
      paymentMethod: "Credit",
      paymentDate: 1624291792000,
      amount: 5000,
      firstName: "Joseph",
      lastName: "Brophy",
      addressLine1: "108 Granger St",
      addressLine2: "",
      city: "Quincy",
      state: "MA",
      postalCode: "02170",
      entityType: "Ind",
      emailAddress: "josephfbrophy@comcast.net",
      employer: "Not Employed",
      occupation: "Not Employed",
      phoneNumber: "6174715353",
      checkNumber: "400330883",
      processorFeeData: {
        amount: 198,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1624766400000,
        paymentMethod: "Check",
        checkNumber: "400330883",
      },
    },
  },
  {
    result: "Created",
    externalContribution: {
      id: "AB199632095",
      recipientId: "77822",
      source: "ActBlue",
      paymentDate: 1624813989000,
      emailAddress: "meggcar@msn.com",
      amount: 10000,
      firstName: "Meggan",
      lastName: "McCarthy",
      addressLine1: "75 Forbes Hill Rd",
      addressLine2: "",
      city: "Quincy",
      state: "MA",
      country: "United States",
      postalCode: "02170",
      phoneNumber: "4022024568",
      payoutId: "1357955",
      payoutDate: 1624766400000,
      occupation: "Care Manager",
      employer: "John Hancock",
      refCode: "",
      entityType: "Ind",
      paymentMethod: "Credit",
      checkNumber: "400330883",
      processorFeeData: {
        amount: 395,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1624766400000,
        paymentMethod: "Check",
        checkNumber: "400330883",
      },
    },
    feeTransaction: {
      id: "1638467981848-b7mLxd",
      source: "ActBlue",
      amount: 395,
      entityName: "ActBlue Technical Services",
      addressLine1: "366 Summer Street",
      city: "Somerville",
      state: "MA",
      postalCode: "02144-3132",
      country: "US",
      paymentDate: 1624766400000,
      paymentMethod: "Check",
      checkNumber: "400330883",
      committeeId: "rec-txns-1638467958340-rdck6I",
      direction: "Out",
      bankVerified: false,
      ruleVerified: true,
      initiatedTimestamp: 1624813989000,
      transactionType: "Disbursement",
      isSubcontracted: false,
      isPartialPayment: false,
      isExistingLiability: false,
      purposeCode: "FUNDR",
      feeForTxn: "1638467981536-JBf56W",
      externalTransactionId: "AB199632095",
      externalTransactionPayoutId: "1357955",
    },
    contribTransaction: {
      id: "1638467981536-JBf56W",
      createdByUser: "system",
      donorId: "1637334772396-I69sYe",
      donorVerificationScore: 0,
      ruleCode: "[ny][state][democrat][primary][53][][senate][nyboe-2020][ind]",
      initiatedTimestamp: 1638467981536,
      direction: "In",
      transactionType: "Contribution",
      bankVerified: false,
      ruleVerified: true,
      source: "ActBlue",
      externalTransactionId: "AB199632095",
      externalTransactionPayoutId: "1357955",
      processPayment: false,
      committeeId: "rec-txns-1638467958340-rdck6I",
      paymentMethod: "Credit",
      paymentDate: 1624813989000,
      amount: 10000,
      firstName: "Meggan",
      lastName: "McCarthy",
      addressLine1: "75 Forbes Hill Rd",
      addressLine2: "",
      city: "Quincy",
      state: "MA",
      postalCode: "02170",
      entityType: "Ind",
      emailAddress: "meggcar@msn.com",
      employer: "John Hancock",
      occupation: "Care Manager",
      phoneNumber: "4022024568",
      checkNumber: "400330883",
      processorFeeData: {
        amount: 395,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1624766400000,
        paymentMethod: "Check",
        checkNumber: "400330883",
      },
    },
  },
  {
    result: "Created",
    externalContribution: {
      id: "AB199945048",
      recipientId: "77822",
      source: "ActBlue",
      paymentDate: 1625083093000,
      emailAddress: "jphfarrington@yahoo.com",
      amount: 10000,
      firstName: "Juarez",
      lastName: "Farrington",
      addressLine1: "1001 Marina Drive",
      addressLine2: "",
      city: "MA",
      state: "MA",
      country: "United States",
      postalCode: "02171",
      phoneNumber: "6173287579",
      payoutId: "1362624",
      payoutDate: 1625025600000,
      occupation: "Business",
      employer: "PHEN",
      refCode: "",
      entityType: "Ind",
      paymentMethod: "Credit",
      checkNumber: "400335576",
      processorFeeData: {
        amount: 395,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1625025600000,
        paymentMethod: "Check",
        checkNumber: "400335576",
      },
    },
    feeTransaction: {
      id: "1638467983427-dyg7PW",
      source: "ActBlue",
      amount: 395,
      entityName: "ActBlue Technical Services",
      addressLine1: "366 Summer Street",
      city: "Somerville",
      state: "MA",
      postalCode: "02144-3132",
      country: "US",
      paymentDate: 1625025600000,
      paymentMethod: "Check",
      checkNumber: "400335576",
      committeeId: "rec-txns-1638467958340-rdck6I",
      direction: "Out",
      bankVerified: false,
      ruleVerified: true,
      initiatedTimestamp: 1625083093000,
      transactionType: "Disbursement",
      isSubcontracted: false,
      isPartialPayment: false,
      isExistingLiability: false,
      purposeCode: "FUNDR",
      feeForTxn: "1638467983161-IlRhNi",
      externalTransactionId: "AB199945048",
      externalTransactionPayoutId: "1362624",
    },
    contribTransaction: {
      id: "1638467983161-IlRhNi",
      createdByUser: "system",
      donorId: "1637334775835-gVkM6F",
      donorVerificationScore: 0,
      ruleCode: "[ny][state][democrat][primary][53][][senate][nyboe-2020][ind]",
      initiatedTimestamp: 1638467983161,
      direction: "In",
      transactionType: "Contribution",
      bankVerified: false,
      ruleVerified: true,
      source: "ActBlue",
      externalTransactionId: "AB199945048",
      externalTransactionPayoutId: "1362624",
      processPayment: false,
      committeeId: "rec-txns-1638467958340-rdck6I",
      paymentMethod: "Credit",
      paymentDate: 1625083093000,
      amount: 10000,
      firstName: "Juarez",
      lastName: "Farrington",
      addressLine1: "1001 Marina Drive",
      addressLine2: "",
      city: "MA",
      state: "MA",
      postalCode: "02171",
      entityType: "Ind",
      emailAddress: "jphfarrington@yahoo.com",
      employer: "PHEN",
      occupation: "Business",
      phoneNumber: "6173287579",
      checkNumber: "400335576",
      processorFeeData: {
        amount: 395,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1625025600000,
        paymentMethod: "Check",
        checkNumber: "400335576",
      },
    },
  },
  {
    result: "Created",
    externalContribution: {
      id: "AB200325701",
      recipientId: "77822",
      source: "ActBlue",
      paymentDate: 1625748278000,
      emailAddress: "mgc818@live.com",
      amount: 10000,
      firstName: "Maureen",
      lastName: "Glynn",
      addressLine1: "8 Viden Road",
      addressLine2: "",
      city: "Quincy",
      state: "MA",
      country: "United States",
      postalCode: "02169",
      phoneNumber: "6177709516",
      payoutId: "1369711",
      payoutDate: 1625976000000,
      occupation: "Attorney",
      employer: "Murphy Donoghue Partners",
      refCode: "",
      entityType: "Ind",
      paymentMethod: "Credit",
      checkNumber: "400342737",
      processorFeeData: {
        amount: 395,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1625976000000,
        paymentMethod: "Check",
        checkNumber: "400342737",
      },
    },
    feeTransaction: {
      id: "1638467985029-kR4Hzj",
      source: "ActBlue",
      amount: 395,
      entityName: "ActBlue Technical Services",
      addressLine1: "366 Summer Street",
      city: "Somerville",
      state: "MA",
      postalCode: "02144-3132",
      country: "US",
      paymentDate: 1625976000000,
      paymentMethod: "Check",
      checkNumber: "400342737",
      committeeId: "rec-txns-1638467958340-rdck6I",
      direction: "Out",
      bankVerified: false,
      ruleVerified: true,
      initiatedTimestamp: 1625748278000,
      transactionType: "Disbursement",
      isSubcontracted: false,
      isPartialPayment: false,
      isExistingLiability: false,
      purposeCode: "FUNDR",
      feeForTxn: "1638467984760-oNt17b",
      externalTransactionId: "AB200325701",
      externalTransactionPayoutId: "1369711",
    },
    contribTransaction: {
      id: "1638467984760-oNt17b",
      createdByUser: "system",
      donorId: "1637334779818-7reg1Z",
      donorVerificationScore: 0,
      ruleCode: "[ny][state][democrat][primary][53][][senate][nyboe-2020][ind]",
      initiatedTimestamp: 1638467984760,
      direction: "In",
      transactionType: "Contribution",
      bankVerified: false,
      ruleVerified: true,
      source: "ActBlue",
      externalTransactionId: "AB200325701",
      externalTransactionPayoutId: "1369711",
      processPayment: false,
      committeeId: "rec-txns-1638467958340-rdck6I",
      paymentMethod: "Credit",
      paymentDate: 1625748278000,
      amount: 10000,
      firstName: "Maureen",
      lastName: "Glynn",
      addressLine1: "8 Viden Road",
      addressLine2: "",
      city: "Quincy",
      state: "MA",
      postalCode: "02169",
      entityType: "Ind",
      emailAddress: "mgc818@live.com",
      employer: "Murphy Donoghue Partners",
      occupation: "Attorney",
      phoneNumber: "6177709516",
      checkNumber: "400342737",
      processorFeeData: {
        amount: 395,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1625976000000,
        paymentMethod: "Check",
        checkNumber: "400342737",
      },
    },
  },
  {
    result: "Created",
    externalContribution: {
      id: "AB200356491",
      recipientId: "77822",
      source: "ActBlue",
      paymentDate: 1625782635000,
      emailAddress: "epod9@comcast.net",
      amount: 5000,
      firstName: "Ellen",
      lastName: "ODonnell",
      addressLine1: "6 Hatherly Road",
      addressLine2: "",
      city: "Quincy",
      state: "MA",
      country: "United States",
      postalCode: "02170",
      phoneNumber: "6178283254",
      payoutId: "1369711",
      payoutDate: 1625976000000,
      occupation: "Realtor",
      employer: "Allison James",
      refCode: "",
      entityType: "Ind",
      paymentMethod: "Credit",
      checkNumber: "400342737",
      processorFeeData: {
        amount: 198,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1625976000000,
        paymentMethod: "Check",
        checkNumber: "400342737",
      },
    },
    feeTransaction: {
      id: "1638467986583-FR3B6n",
      source: "ActBlue",
      amount: 198,
      entityName: "ActBlue Technical Services",
      addressLine1: "366 Summer Street",
      city: "Somerville",
      state: "MA",
      postalCode: "02144-3132",
      country: "US",
      paymentDate: 1625976000000,
      paymentMethod: "Check",
      checkNumber: "400342737",
      committeeId: "rec-txns-1638467958340-rdck6I",
      direction: "Out",
      bankVerified: false,
      ruleVerified: true,
      initiatedTimestamp: 1625782635000,
      transactionType: "Disbursement",
      isSubcontracted: false,
      isPartialPayment: false,
      isExistingLiability: false,
      purposeCode: "FUNDR",
      feeForTxn: "1638467986320-U1oiid",
      externalTransactionId: "AB200356491",
      externalTransactionPayoutId: "1369711",
    },
    contribTransaction: {
      id: "1638467986320-U1oiid",
      createdByUser: "system",
      donorId: "1637334783608-GK5skA",
      donorVerificationScore: 0,
      ruleCode: "[ny][state][democrat][primary][53][][senate][nyboe-2020][ind]",
      initiatedTimestamp: 1638467986320,
      direction: "In",
      transactionType: "Contribution",
      bankVerified: false,
      ruleVerified: true,
      source: "ActBlue",
      externalTransactionId: "AB200356491",
      externalTransactionPayoutId: "1369711",
      processPayment: false,
      committeeId: "rec-txns-1638467958340-rdck6I",
      paymentMethod: "Credit",
      paymentDate: 1625782635000,
      amount: 5000,
      firstName: "Ellen",
      lastName: "ODonnell",
      addressLine1: "6 Hatherly Road",
      addressLine2: "",
      city: "Quincy",
      state: "MA",
      postalCode: "02170",
      entityType: "Ind",
      emailAddress: "epod9@comcast.net",
      employer: "Allison James",
      occupation: "Realtor",
      phoneNumber: "6178283254",
      checkNumber: "400342737",
      processorFeeData: {
        amount: 198,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1625976000000,
        paymentMethod: "Check",
        checkNumber: "400342737",
      },
    },
  },
  {
    result: "Created",
    externalContribution: {
      id: "AB200361761",
      recipientId: "77822",
      source: "ActBlue",
      paymentDate: 1625787364000,
      emailAddress: "therlihy01@comcast.net",
      amount: 5000,
      firstName: "Tom",
      lastName: "Herlihy",
      addressLine1: "73 south central ave",
      addressLine2: "",
      city: "Quincy",
      state: "MA",
      country: "United States",
      postalCode: "02170-4018",
      phoneNumber: "6178345082",
      payoutId: "1369711",
      payoutDate: 1625976000000,
      occupation: "Telecommunications",
      employer: "Siena Engineering Group",
      refCode: "",
      entityType: "Ind",
      paymentMethod: "Credit",
      checkNumber: "400342737",
      processorFeeData: {
        amount: 198,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1625976000000,
        paymentMethod: "Check",
        checkNumber: "400342737",
      },
    },
    feeTransaction: {
      id: "1638467988097-2yNCZp",
      source: "ActBlue",
      amount: 198,
      entityName: "ActBlue Technical Services",
      addressLine1: "366 Summer Street",
      city: "Somerville",
      state: "MA",
      postalCode: "02144-3132",
      country: "US",
      paymentDate: 1625976000000,
      paymentMethod: "Check",
      checkNumber: "400342737",
      committeeId: "rec-txns-1638467958340-rdck6I",
      direction: "Out",
      bankVerified: false,
      ruleVerified: true,
      initiatedTimestamp: 1625787364000,
      transactionType: "Disbursement",
      isSubcontracted: false,
      isPartialPayment: false,
      isExistingLiability: false,
      purposeCode: "FUNDR",
      feeForTxn: "1638467987827-kGHGiv",
      externalTransactionId: "AB200361761",
      externalTransactionPayoutId: "1369711",
    },
    contribTransaction: {
      id: "1638467987827-kGHGiv",
      createdByUser: "system",
      donorId: "1637334787342-QkyYYI",
      donorVerificationScore: 0,
      ruleCode: "[ny][state][democrat][primary][53][][senate][nyboe-2020][ind]",
      initiatedTimestamp: 1638467987827,
      direction: "In",
      transactionType: "Contribution",
      bankVerified: false,
      ruleVerified: true,
      source: "ActBlue",
      externalTransactionId: "AB200361761",
      externalTransactionPayoutId: "1369711",
      processPayment: false,
      committeeId: "rec-txns-1638467958340-rdck6I",
      paymentMethod: "Credit",
      paymentDate: 1625787364000,
      amount: 5000,
      firstName: "Tom",
      lastName: "Herlihy",
      addressLine1: "73 south central ave",
      addressLine2: "",
      city: "Quincy",
      state: "MA",
      postalCode: "02170-4018",
      entityType: "Ind",
      emailAddress: "therlihy01@comcast.net",
      employer: "Siena Engineering Group",
      occupation: "Telecommunications",
      phoneNumber: "6178345082",
      checkNumber: "400342737",
      processorFeeData: {
        amount: 198,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1625976000000,
        paymentMethod: "Check",
        checkNumber: "400342737",
      },
    },
  },
  {
    result: "Created",
    externalContribution: {
      id: "AB200393582",
      recipientId: "77822",
      source: "ActBlue",
      paymentDate: 1625851760000,
      emailAddress: "jenhop10@gmail.com",
      amount: 10000,
      firstName: "Jennifer",
      lastName: "Hopkins",
      addressLine1: "75 Myopia Road",
      addressLine2: "",
      city: "Quincy",
      state: "MA",
      country: "United States",
      postalCode: "02169",
      phoneNumber: "",
      payoutId: "1369711",
      payoutDate: 1625976000000,
      occupation: "Consultant",
      employer: "Willmott Associates",
      refCode: "",
      entityType: "Ind",
      paymentMethod: "Credit",
      checkNumber: "400342737",
      processorFeeData: {
        amount: 395,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1625976000000,
        paymentMethod: "Check",
        checkNumber: "400342737",
      },
    },
    feeTransaction: {
      id: "1638467989877-t68nuA",
      source: "ActBlue",
      amount: 395,
      entityName: "ActBlue Technical Services",
      addressLine1: "366 Summer Street",
      city: "Somerville",
      state: "MA",
      postalCode: "02144-3132",
      country: "US",
      paymentDate: 1625976000000,
      paymentMethod: "Check",
      checkNumber: "400342737",
      committeeId: "rec-txns-1638467958340-rdck6I",
      direction: "Out",
      bankVerified: false,
      ruleVerified: true,
      initiatedTimestamp: 1625851760000,
      transactionType: "Disbursement",
      isSubcontracted: false,
      isPartialPayment: false,
      isExistingLiability: false,
      purposeCode: "FUNDR",
      feeForTxn: "1638467989439-LDMNj7",
      externalTransactionId: "AB200393582",
      externalTransactionPayoutId: "1369711",
    },
    contribTransaction: {
      id: "1638467989439-LDMNj7",
      createdByUser: "system",
      donorId: "1637334790613-oERYMa",
      donorVerificationScore: 0,
      ruleCode: "[ny][state][democrat][primary][53][][senate][nyboe-2020][ind]",
      initiatedTimestamp: 1638467989439,
      direction: "In",
      transactionType: "Contribution",
      bankVerified: false,
      ruleVerified: true,
      source: "ActBlue",
      externalTransactionId: "AB200393582",
      externalTransactionPayoutId: "1369711",
      processPayment: false,
      committeeId: "rec-txns-1638467958340-rdck6I",
      paymentMethod: "Credit",
      paymentDate: 1625851760000,
      amount: 10000,
      firstName: "Jennifer",
      lastName: "Hopkins",
      addressLine1: "75 Myopia Road",
      addressLine2: "",
      city: "Quincy",
      state: "MA",
      postalCode: "02169",
      entityType: "Ind",
      emailAddress: "jenhop10@gmail.com",
      employer: "Willmott Associates",
      occupation: "Consultant",
      phoneNumber: "",
      checkNumber: "400342737",
      processorFeeData: {
        amount: 395,
        entityName: "ActBlue Technical Services",
        addressLine1: "366 Summer Street",
        city: "Somerville",
        state: "MA",
        postalCode: "02144-3132",
        country: "US",
        paymentDate: 1625976000000,
        paymentMethod: "Check",
        checkNumber: "400342737",
      },
    },
  },
];
