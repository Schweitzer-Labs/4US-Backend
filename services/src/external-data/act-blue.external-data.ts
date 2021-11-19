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
import { State } from "../utils/enums/state.enum";
import { Source } from "../utils/enums/source.enum";
import { EntityType } from "../utils/enums/entity-type.enum";
import { getCommitteeByActBlueAccountIdAndDecode } from "../utils/model/committee/get-committee-by-actblue-id.utils";
import { syncExternalContributions } from "../pipes/external-txns-to-ddb.pipe";
import { Stripe } from "stripe";
import { ILexisNexisConfig } from "../clients/lexis-nexis/lexis-nexis.client";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { PaymentMethod } from "../utils/enums/payment-method.enum";
import { dollarStrToCents } from "../utils/cents.util";

const committeeGetter: CommitteeGetter =
  getCommitteeByActBlueAccountIdAndDecode;

const contributionMapper: ContributionMapper = (
  ab: IActBluePaidContribution
): IExternalContrib => ({
  id: ab["Receipt ID"],
  recipientId: ab["Recipient ID"],
  source: Source.ActBlue,
  paymentDate: formatDate(ab["Date"]),
  emailAddress: ab["Donor Email"],
  amount: dollarStrToCents(ab["Amount"]),
  firstName: ab["Donor First Name"],
  lastName: ab["Donor Last Name"],
  addressLine1: ab["Donor Addr1"],
  addressLine2: ab["Donor Addr2"],
  city: ab["Donor City"],
  state: ab["Donor State"],
  country: ab["Donor Country"],
  postalCode: ab["Donor ZIP"],
  phoneNumber: ab["Donor Phone"],
  payoutId: ab["Disbursement ID"],
  payoutDate: formatDate(ab["Disbursement Date"]),
  occupation: ab["Donor Occupation"],
  employer: ab["Donor Employer"],
  refCode: ab["Reference Code"],
  entityType: EntityType.Ind,
  paymentMethod: PaymentMethod.Credit,
  checkNumber: ab["Check Number"],
  processorFeeData: {
    amount: dollarStrToCents(ab["Fee"]),
    entityName: "ActBlue Technical Services",
    addressLine1: "366 Summer Street",
    city: "Somerville",
    state: State.MA,
    postalCode: "02144-3132",
    country: "US",
    paymentDate: formatDate(ab["Disbursement Date"]),
  },
});

const isNewValidator: IsNewValidator = isNewActBlueTxn;

// @Todo refactor with currying
// https://samhh.github.io/fp-ts-std/modules/Function.ts.html
export const syncActBlue =
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
    syncExternalContributions({
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
