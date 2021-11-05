import { DynamoDB } from "aws-sdk";
import { pipe } from "fp-ts/function";
import { getCommitteeById } from "../utils/model/committee/get-committee-by-id.query";
import { right, TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { IContribution } from "./event-to-contribution";
import { taskEither } from "fp-ts";
import { ICommittee } from "../model/committee.type";

export interface ICommitteeContribution {
  committee: ICommittee;
  contribution: IContribution;
}

export const contributionToCommitteeContribution =
  (committeeTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (
    contribution: IContribution
  ): TaskEither<ApplicationError, ICommitteeContribution> => {
    return pipe(
      getCommitteeById(committeeTableName)(dynamoDB)(contribution.committeeId),
      taskEither.chain(committeeToCommitteeContribution(contribution))
    );
  };

const committeeToCommitteeContribution =
  (contribution: IContribution) =>
  (
    committee: ICommittee
  ): TaskEither<ApplicationError, ICommitteeContribution> => {
    return right({
      contribution,
      committee,
    });
  };
