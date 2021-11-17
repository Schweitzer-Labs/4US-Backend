import {
  CommitteeGetter,
  ContributionMapper,
  IsNewValidator,
  toDeps,
} from "../model/external-data.type";
import { IActBluePaidContribution } from "../clients/actblue/actblue.decoders";
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
import {syncExternalContributions} from "../pipes/external-txns-to-ddb.pipe";

const formatDate = (dateStr: string): number => new Date(dateStr).getTime();

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
  fee: dollarStrToCents(ab["Fee"]),
  occupation: ab["Donor Occupation"],
  employer: ab["Donor Employer"],
  refCode: ab["Reference Code"],
  entityType: EntityType.Ind,
});

const isNewValidator: IsNewValidator = isNewActBlueTxn;

export const syncActBlue = flow(
  toDeps<IActBluePaidContribution>(committeeGetter)(contributionMapper)(
    isNewValidator
  ),
  syncExternalContributions<IActBluePaidContribution>
);
