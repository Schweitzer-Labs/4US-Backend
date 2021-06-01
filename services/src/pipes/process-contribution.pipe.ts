import { IComplianceResult } from "./compliance-check.pipe";
import { PaymentMethod } from "../utils/enums/payment-method.enum";
import { pipe } from "fp-ts/function";
import { Stripe } from "stripe";
import { committeeContributionToPayment } from "../either-tasks/contribution-to-payment";
import { taskEither } from "fp-ts";
import { paymentToDDB } from "../either-tasks/payment-to-ddb";
import { DynamoDB } from "aws-sdk";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { ITransaction } from "../queries/search-transactions.decoder";
import { genTxnId } from "../utils/gen-txn-id.utils";
import { Direction } from "../utils/enums/direction.enum";
import { now } from "../utils/time.utils";
import { Source } from "../utils/enums/source.enum";
import { TransactionType } from "../utils/enums/transaction-type.enum";
import { putTransaction } from "../utils/model/put-transaction.utils";

export const processContribution =
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (stripe: Stripe) =>
  ({
    createContributionInput: c,
    committee,
    donor,
    rule,
  }: IComplianceResult): TaskEither<ApplicationError, ITransaction> => {
    switch (c.paymentMethod) {
      case PaymentMethod.Credit:
      case PaymentMethod.Debit:
        return pipe(
          committeeContributionToPayment(stripe)({
            committee,
            contribution: {
              donorId: donor.id,
              ruleCode: rule.code,
              // @ToDo Make source dynamic to support email on public donations
              source: Source.DASHBOARD,
              ...c,
            },
          }),
          taskEither.chain(paymentToDDB(txnsTableName)(dynamoDB))
        );
      default:
        const txn: ITransaction = {
          // required field
          id: genTxnId(),
          direction: Direction.IN,
          transactionType: TransactionType.CONTRIBUTION,
          bankVerified: false,
          ruleVerified: true,
          initiatedTimestamp: now(),
          source: Source.DASHBOARD,
          ...c,
        };
        return pipe(
          taskEither.tryCatch(
            () => putTransaction(txnsTableName)(dynamoDB)(txn),
            (e) =>
              new ApplicationError(
                "Failed to write rules compliant transaction to DDB.",
                e
              )
          )
        );
    }
  };
