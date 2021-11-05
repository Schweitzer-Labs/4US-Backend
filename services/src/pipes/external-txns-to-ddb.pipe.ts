import { DynamoDB } from "aws-sdk";
import { ICommittee } from "../model/committee.type";
import { pipe } from "fp-ts/function";
import { IExternalData, IExternalTxn } from "../model/external-data.type";
import { ApplicationError } from "../utils/application-error";
import { TaskEither } from "fp-ts/TaskEither";
import { taskEither } from "fp-ts";
import { CreateContributionInput } from "../graphql/input-types/create-contribution.input-type";
import { getCommitteesByActBlueIdAndDecode } from "../utils/model/committee/get-committee-by-actblue-id.utils";
import { getOneFromList } from "../utils/get-one-from-list.utils";
import { PaymentMethod } from "../utils/enums/payment-method.enum";
import { EntityType } from "../utils/enums/entity-type.enum";

const extTxnToCreateContribInput =
  (com: ICommittee) =>
  (extTxn: IExternalTxn): CreateContributionInput => ({
    committeeId: com.id,
    paymentMethod: PaymentMethod.Credit,
    processPayment: false,
    paymentDate: extTxn.paymentDate,
    amount: extTxn.amount,
    firstName: extTxn.firstName,
    middleName: extTxn.middleName,
    lastName: extTxn.lastName,
    addressLine1: extTxn.addressLine1,
    addressLine2: extTxn.addressLine2,
    city: extTxn.city,
    state: extTxn.state,
    postalCode: extTxn.postalCode,
    entityType: EntityType.Ind,
    emailAddress: extTxn.emailAddress,
    employer: extTxn.employer,
    employmentStatus: extTxn.employmentStatus,
    occupation: extTxn.occupation,
    phoneNumber: extTxn.phoneNumber,
  });

const externalDataToCreateContribInput =
  (extData: IExternalData) =>
  (com: ICommittee): CreateContributionInput[] =>
    extData.transactions.map(extTxnToCreateContribInput(com));

export const externalTxnsToDdb =
  (comTable: string) =>
  (txnsTable: string) =>
  (ddb: DynamoDB) =>
  ({ transactions }: IExternalData): TaskEither<ApplicationError, boolean> =>
    pipe(
      // getOneFromList<IExternalTxn>(transactions),
      // taskEither.map((extTxn) => extTxn.recipientGovId),
      // taskEither.chain(getCommitteesByActBlueIdAndDecode(comTable)(ddb)),
      // taskEither.map(externalDataToCreateContribInput(transactions)),
      taskEither.of(true)
    );
