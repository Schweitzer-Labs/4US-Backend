import { ICommittee } from "../../src/queries/get-committee-by-id.query";
import { Plan } from "../../src/utils/enums/plan.enum";

export const qaCommitteesData: ICommittee[] = [
  {
    // required
    id: "angel-cruz",
    committeeName: "Angel Cruz for Bronx Judge",
    bankName: "other bank",
    candidateFirstName: "Angel",
    candidateLastName: "Cruz",
    stripeAccount: "acct_1IjTcsRC8iiQex3V",
    emailAddresses: "evan@schweitzerlabs.com,seemant@schweitzerlabs.com",
    platformPlan: Plan.FourUs,
    members: [
      "james-martin",
      "380d0179-d813-445d-9032-fc25249b4de7",
      "41613d64-a542-4d5d-84f0-372fbdb94326",
      "86450595-2de6-492b-af6a-5597161ebcda",
    ],
    tzDatabaseName: "America/New_York",
    state: "ny",
    scope: "state",
    party: "democrat",
    race: "general",
    district: "35",
    county: "",
    officeType: "assembly",
    ruleVersion: "nyboe-2020",
  },
  {
    // required
    id: "john-safford",
    committeeName: "John Safford for Supervisor",
    bankName: "adirondack trust company",
    candidateFirstName: "John",
    candidateLastName: "Safford",
    stripeAccount: "acct_1IjTcsRC8iiQex3V",
    emailAddresses: "evan@schweitzerlabs.com,seemant@schweitzerlabs.com",
    platformPlan: Plan.FourUs,
    members: [
      "james-martin",
      "380d0179-d813-445d-9032-fc25249b4de7",
      "41613d64-a542-4d5d-84f0-372fbdb94326",
      "86450595-2de6-492b-af6a-5597161ebcda",
    ],
    tzDatabaseName: "America/New_York",
    state: "ny",
    scope: "local",
    party: "republican",
    race: "general",
    district: "",
    county: "saratoga",
    officeType: "supervisor",
    ruleVersion: "nyboe-2020",
    finicityCustomerId: "5005871881",
    finicityAccountId: "5012575024",
  },
  {
    // required
    id: "will-schweitzer",
    committeeName: "Will Schweitzer for Supervisor",
    bankName: "chase",
    candidateFirstName: "Will",
    candidateLastName: "Schweitzer",
    stripeAccount: "acct_1IjTcsRC8iiQex3V",
    emailAddresses: "evan@schweitzerlabs.com,seemant@schweitzerlabs.com",
    platformPlan: Plan.FourUs,
    members: [
      "james-martin",
      "380d0179-d813-445d-9032-fc25249b4de7",
      "41613d64-a542-4d5d-84f0-372fbdb94326",
      "86450595-2de6-492b-af6a-5597161ebcda",
    ],
    tzDatabaseName: "America/New_York",
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
  },
  {
    // required
    id: "evan-piro",
    committeeName: "Evan Piro for Mayor",
    bankName: "chase",
    candidateFirstName: "Evan",
    candidateLastName: "Piro",
    stripeAccount: "acct_1IjTcsRC8iiQex3V",
    emailAddresses: "evan@schweitzerlabs.com",
    platformPlan: Plan.FourUs,
    members: [
      "james-martin",
      "380d0179-d813-445d-9032-fc25249b4de7",
      "41613d64-a542-4d5d-84f0-372fbdb94326",
      "86450595-2de6-492b-af6a-5597161ebcda",
    ],
    tzDatabaseName: "America/New_York",
    state: "ny",
    scope: "state",
    party: "democrat",
    race: "general",
    district: "35",
    county: "",
    officeType: "assembly",
    ruleVersion: "nyboe-2020",
    finicityCustomerId: "5007489410",
    finicityAccountId: "5016000964",
  },
  {
    // required
    id: "nelson-lopez",
    committeeName: "Nelson Lopez for Mayor",
    bankName: "chase",
    candidateFirstName: "Nelson",
    candidateLastName: "Lopez",
    stripeAccount: "acct_1IjTcsRC8iiQex3V",
    emailAddresses: "evan@schweitzerlabs.com",
    platformPlan: Plan.FourUs,
    members: [
      "james-martin",
      "380d0179-d813-445d-9032-fc25249b4de7",
      "41613d64-a542-4d5d-84f0-372fbdb94326",
      "86450595-2de6-492b-af6a-5597161ebcda",
    ],
    tzDatabaseName: "America/New_York",
    state: "ny",
    scope: "state",
    party: "democrat",
    race: "general",
    district: "35",
    county: "",
    officeType: "assembly",
    ruleVersion: "nyboe-2020",
    finicityCustomerId: "5007489410",
    finicityAccountId: "5016000964",
  },
  {
    // required
    id: "seemant-kulleen",
    committeeName: "Seemant Kulleen for Mayor",
    bankName: "chase",
    candidateFirstName: "Seemant",
    candidateLastName: "Kulleen",
    stripeAccount: "acct_1IjTcsRC8iiQex3V",
    emailAddresses: "evan@schweitzerlabs.com",
    platformPlan: Plan.FourUs,
    members: [
      "james-martin",
      "380d0179-d813-445d-9032-fc25249b4de7",
      "41613d64-a542-4d5d-84f0-372fbdb94326",
      "86450595-2de6-492b-af6a-5597161ebcda",
    ],
    tzDatabaseName: "America/New_York",
    state: "ny",
    scope: "state",
    party: "democrat",
    race: "general",
    district: "35",
    county: "",
    officeType: "assembly",
    ruleVersion: "nyboe-2020",
    finicityCustomerId: "5007489410",
    finicityAccountId: "5016000964",
  },
];