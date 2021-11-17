import {
  CommitteeGetter,
  ContributionMapper,
  IExternalContrib,
  IExternalTxnsToDDBDeps,
  IsNewValidator,
} from "../model/external-data.type";
import {
  formatDate,
  IActBluePaidContribution,
} from "../clients/actblue/actblue.decoders";
import { DynamoDB } from "aws-sdk";
import {
  IGetTxnByActBlueTxnIdArgs,
  isNewActBlueTxn,
} from "../utils/model/transaction/get-txn-by-actblue-id.utils";
import * as t from "io-ts";
import { fromEnum } from "../utils/from-enum.utils";
import { State } from "../utils/enums/state.enum";
import { Source } from "../utils/enums/source.enum";
import { EntityType } from "../utils/enums/entity-type.enum";
import { flow } from "fp-ts/function";
import { getCommitteeByActBlueAccountIdAndDecode } from "../utils/model/committee/get-committee-by-actblue-id.utils";
import { syncExternalContributions } from "../pipes/external-txns-to-ddb.pipe";
import { Stripe } from "stripe";
import { ILexisNexisConfig } from "../clients/lexis-nexis/lexis-nexis.client";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { PaymentMethod } from "../utils/enums/payment-method.enum";

const committeeGetter: CommitteeGetter =
  getCommitteeByActBlueAccountIdAndDecode;

const contributionMapper: ContributionMapper<IActBluePaidContribution> = (
  ab
) => ({
  id: ab["Receipt ID"],
  recipientId: ab["Recipient ID"],
  source: Source.ActBlue,
  paymentDate: formatDate(ab["Date"]),
  amount: dollarStrToCents(ab["Amount"]),
  firstName: ab["Donor First Name"],
  lastName: ab["Donor Last Name"],
  addressLine1: ab["Donor Addr1"],
  addressLine2: ab["Donor Addr2"],
  city: ab["Donor City"],
  state: ab["Donor State"],
  country: ab["Donor Country"],
  postalCode: ab["Donor ZIP"],
  payoutId: ab["Disbursement ID"],
  payoutDate: formatDate(ab["Disbursement Date"]),
  occupation: ab["Donor Occupation"],
  employer: ab["Donor Employer"],
  refCode: ab["Reference Code"],
  entityType: EntityType.Ind,
  paymentMethod: PaymentMethod.Credit,
  checkNumber: ab["Check Number"],
  processorFee: dollarStrToCents(ab["Fee"]),
  processorEntityName: "ActBlue Technical Services",
  processorAddressLine1: "366 Summer Street",
  processorCity: "Somerville",
  processorState: State.MA,
  processorPostalCode: "02144-3132",
  processorCountry: "US",
});

const isNewValidator: IsNewValidator = isNewActBlueTxn;

// @Todo refactor with currying
// https://samhh.github.io/fp-ts-std/modules/Function.ts.html
export const syncActBlue =
  <IActBluePaidContribution>(committeeGetter: CommitteeGetter) =>
  (contributionMapper: ContributionMapper<IActBluePaidContribution>) =>
  (isNewValidator: IsNewValidator) =>
  (committeesTable: string) =>
  (billableEventsTable: string) =>
  (donorsTableName: string) =>
  (transactionsTableName: string) =>
  (rulesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (stripe: Stripe) =>
  (lexisNexisConfig: ILexisNexisConfig) =>
  (
    actBlueContribs: IActBluePaidContribution[]
  ): TaskEither<ApplicationError, IExternalContrib[]> =>
    syncExternalContributions<IActBluePaidContribution>({
      committeesTable,
      billableEventsTable,
      donorsTableName,
      transactionsTableName,
      rulesTableName,
      dynamoDB,
      stripe,
      lexisNexisConfig,
      committeeGetter,
      contributionMapper,
      isNewValidator,
    })(actBlueContribs);
