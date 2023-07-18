import { DynamoDB } from "aws-sdk";
import * as dotenv from "dotenv";
import { genCommittee } from "../utils/gen-committee.util";
import { putCommittee } from "../../src/utils/model/committee/put-committee.utils";

dotenv.config();

const comTable = process.env.COMMITTEES_DDB_TABLE_NAME;
const ddb = new DynamoDB();

const run = async () => {
  const com = genCommittee({
    state: "ny",
    scope: "local",
    party: "republican",
    race: "general",
    district: "",
    county: "saratoga",
    officeType: "supervisor",
    ruleVersion: "nyboe-2020",
    finicityCustomerId: "5007489410",
    finicityAccountId: "5016000964",
    efsElectionId: 139,
  });

  console.log(com.id);

  await putCommittee(comTable)(ddb)(com);
};

run();
