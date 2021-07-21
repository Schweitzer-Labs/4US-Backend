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
  (currentUser: string) =>
  (txnsTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (stripe: Stripe) =>
  (
    complianceResult: IComplianceResult
  ): TaskEither<ApplicationError, ITransaction> => {
    const {
      createContributionInput: c,
      committee,
      donor,
      rule,
    } = complianceResult;

    console.log(
      "Contribution processing pipe called.",
      JSON.stringify(complianceResult)
    );
    const baseTxn = {
      id: genTxnId(),
      createdByUser: currentUser,
      donorId: donor.id,
      donorVerificationScore: donor.instantIdComprehensiveVerificationScore,
      ruleCode: rule?.code,
      initiatedTimestamp: now(),
      direction: Direction.In,
      transactionType: TransactionType.Contribution,
      bankVerified: false,
      ruleVerified: rule?.code ? true : false,
      source: Source.DASHBOARD,
      ...c,
    };

    switch (c.paymentMethod) {
      case PaymentMethod.Credit:
      case PaymentMethod.Debit:
        return pipe(
          committeeContributionToPayment(stripe)({
            committee,
            contribution: {
              ...baseTxn,
            },
          }),
          taskEither.chain(paymentToDDB(txnsTableName)(dynamoDB))
        );
      default:
        return pipe(
          taskEither.tryCatch(
            () => putTransaction(txnsTableName)(dynamoDB)(baseTxn),
            (e) =>
              new ApplicationError(
                "Failed to write rules compliant transaction to DDB.",
                e
              )
          )
        );
    }
  };
