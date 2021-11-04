import { DynamoDB } from "aws-sdk";
import { ITransaction } from "../../types/transaction.type";
import { genTxnId } from "../../utils/gen-txn-id.utils";
import { Direction } from "../../utils/enums/direction.enum";
import { PaymentMethod } from "../../utils/enums/payment-method.enum";
import { now } from "../../utils/time.utils";
import { Source } from "../../utils/enums/source.enum";
import { TransactionType } from "../../utils/enums/transaction-type.enum";
import { validateDemoCommittee } from "./validate-demo-committee.utils";
import { pipe } from "fp-ts/function";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../../utils/application-error";
import { taskEither } from "fp-ts";
import { putTransactionAndDecode } from "../../utils/model/put-transaction.utils";
import { SeedDemoBankRecordsInput } from "../../graphql/input-types/seed-demo-bank-records.input-type";

const toMockDisb = (committeeId: string): ITransaction => {
  const timestamp = 1629975600000;
  const finData = {
    finicityBestRepresentation:
      "ORIG CO NAME INTUIT INC ORIG ID DESC DATE CO ENTRY DESCR SOFTWARE SEC CCD TRACE EED IND ID IND NAME SCHWEITZER LABORATORIE",
    finicityCategory: "Income",
    finicityDescription: "ORIG CO NAME:INTUIT INC",
    finicityNormalizedPayeeName: "Co Intuit Inc",
    finicityPaymentMethod: "Debit",
    finicityPostedDate: timestamp,
    finicityTransactionDate: timestamp,
    finicityTransactionId: 1,
  };
  const amount = 50000;
  return {
    ...finData,
    committeeId,
    id: genTxnId(),
    direction: Direction.Out,
    amount,
    paymentMethod: PaymentMethod.Credit,
    bankVerified: true,
    ruleVerified: false,
    initiatedTimestamp: timestamp,
    paymentDate: timestamp,
    source: Source.FINICITY,
    transactionType: TransactionType.Disbursement,
    finicityTransactionData: {
      id: 1,
      amount,
      accountId: 1,
      status: "complete",
      description: finData.finicityDescription,
      postedDate: timestamp,
      transactionDate: timestamp,
      customerId: 1,
      createdDate: timestamp,
      categorization: {
        normalizedPayeeName: finData.finicityNormalizedPayeeName,
        category: finData.finicityCategory,
        bestRepresentation: finData.finicityBestRepresentation,
        country: "US",
      },
    },
  };
};

const toMockContrib = (committeeId: string): ITransaction => {
  const timestamp = now();
  const amount = 120000;
  const finData = {
    finicityBestRepresentation:
      "ORIG CO NAME INTUIT INC ORIG ID DESC DATE CO ENTRY DESCR SOFTWARE SEC CCD TRACE EED IND ID IND NAME SCHWEITZER LABORATORIE",
    finicityCategory: "Income",
    finicityDescription: "Deposit",
    finicityNormalizedPayeeName: "deposit",
    finicityPaymentMethod: "Credit",
    finicityPostedDate: timestamp,
    finicityTransactionDate: timestamp,
    finicityTransactionId: 1,
  };
  return {
    ...finData,
    committeeId,
    id: genTxnId(),
    direction: Direction.In,
    amount,
    paymentMethod: PaymentMethod.Credit,
    bankVerified: true,
    ruleVerified: false,
    initiatedTimestamp: timestamp,
    paymentDate: timestamp,
    source: Source.FINICITY,
    transactionType: TransactionType.Contribution,
    finicityTransactionData: {
      id: 1,
      amount,
      accountId: 1,
      status: "complete",
      description: finData.finicityDescription,
      postedDate: timestamp,
      transactionDate: timestamp,
      customerId: 1,
      createdDate: timestamp,
      categorization: {
        normalizedPayeeName: finData.finicityNormalizedPayeeName,
        category: finData.finicityCategory,
        bestRepresentation: finData.finicityBestRepresentation,
        country: "US",
      },
    },
  };
};

export const seedTxn =
  (txnTable: string) =>
  (ddb: DynamoDB) =>
  (s: SeedDemoBankRecordsInput): TaskEither<ApplicationError, ITransaction> =>
    pipe(
      validateDemoCommittee(s.committeeId),
      taskEither.map(
        s.transactionType === TransactionType.Contribution
          ? toMockContrib
          : toMockDisb
      ),
      taskEither.chain(putTransactionAndDecode(txnTable)(ddb))
    );
