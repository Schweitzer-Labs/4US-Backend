import { DynamoDB } from "aws-sdk";
import { ICommittee } from "./get-committee-by-id.query";
import { committeeAndDonorToRuleCode } from "../utils/model/gen-rule-code.utils";
import { TaskEither, tryCatch } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import { pipe } from "fp-ts/function";
import { StatusCodes } from "http-status-codes";
import { taskEither } from "fp-ts";
import { validateDDBResponse } from "../repositories/ddb.utils";
import { IRule, Rules } from "./get-rule.decoder";
import { IDonor } from "./search-donors.decoder";

export const queryDDB =
  (rulesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committee: ICommittee) =>
  async (donor: IDonor): Promise<unknown> => {
    const res = await dynamoDB
      .query({
        TableName: rulesTableName,
        KeyConditionExpression: "code = :code",
        ExpressionAttributeValues: {
          ":code": { S: committeeAndDonorToRuleCode(committee)(donor) },
        },
      })
      .promise();

    return res.Items.map((item) => DynamoDB.Converter.unmarshall(item));
  };

const RulesToRule = (rules: IRule[]): TaskEither<ApplicationError, IRule> => {
  if (rules.length > 0) {
    return taskEither.right(rules[0]);
  } else {
    return taskEither.left(
      new ApplicationError("Rule not found", {}, StatusCodes.NOT_FOUND)
    );
  }
};

export const committeeAndDonorToRule =
  (rulesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (committee: ICommittee) =>
  (donor: IDonor): TaskEither<ApplicationError, IRule> => {
    return pipe(
      tryCatch<ApplicationError, any>(
        () => queryDDB(rulesTableName)(dynamoDB)(committee)(donor),
        (e) =>
          new ApplicationError(
            "Get rule request failed",
            e,
            StatusCodes.INTERNAL_SERVER_ERROR
          )
      ),
      taskEither.chain(validateDDBResponse(Rules)),
      taskEither.chain(RulesToRule)
    );
  };
