import { IExternalContrib } from "../../src/model/external-data.type";
import {
  actBlueCSVMetadataToTypedData,
  getActBlueCSVMetadata,
} from "../../src/clients/actblue/actblue.client";
import { isLeft } from "fp-ts/Either";
import { sleep } from "../../src/utils/sleep.utils";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { mLog } from "../../src/utils/m-log.utils";
import { syncActBlue } from "../../src/external-data/act-blue.external-data";
import { ActBlueCSVType } from "../../src/clients/actblue/actblue.decoders";
import { ICommittee } from "../../src/model/committee.type";
import { nMonthsAgo, now } from "../../src/utils/time.utils";
import { ILexisNexisConfig } from "../../src/clients/lexis-nexis/lexis-nexis.client";
import { DynamoDB } from "aws-sdk";
import { Stripe } from "stripe";

interface Args {
  committee: ICommittee;
  lnConfig: ILexisNexisConfig;
  transactionsTable: string;
  billableEventsTable: string;
  rulesTable: string;
  donorsTable: string;
  committeesTable: string;
  dynamoDB: DynamoDB;
  stripe: Stripe;
}

export const committeeToAC = async ({
  committee,
  lnConfig,
  transactionsTable,
  billableEventsTable,
  rulesTable,
  donorsTable,
  committeesTable,
  dynamoDB,
  stripe,
}: Args): Promise<IExternalContrib[]> => {
  const rn = now();

  const sixMAgo = nMonthsAgo(6)(rn) + 1000 * 60 * 60 * 24;

  const eitherCsvMetadata = await getActBlueCSVMetadata(
    ActBlueCSVType.PaidContributions
  )(committee.actBlueAPICredentials)(sixMAgo)(rn)();

  if (isLeft(eitherCsvMetadata)) throw new Error("ActBlue csv request failed");

  await sleep(12000);

  if (isLeft(eitherCsvMetadata)) throw new Error("ActBlue csv request failed");

  const eitherContribs = await pipe(
    actBlueCSVMetadataToTypedData(eitherCsvMetadata.right.csvId)(
      committee.actBlueAPICredentials
    ),
    taskEither.chain(mLog("csv data parsed")),
    taskEither.chain(
      syncActBlue({
        transactionsTable,
        billableEventsTable,
        rulesTable,
        donorsTable,
        committeesTable,
        dynamoDB: dynamoDB,
        lexisNexisConfig: lnConfig,
        stripe,
      })(committee.id)
    )
  )();

  if (isLeft(eitherContribs)) throw new Error("ActBlue committee sync failed");

  console.log("res is here");

  console.log(eitherContribs.right);

  return eitherContribs.right;
};
