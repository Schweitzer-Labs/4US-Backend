import { pipe } from "fp-ts/function";
import { DynamoDB } from "aws-sdk";
import { TaskEither } from "fp-ts/TaskEither";
import { ApplicationError } from "../utils/application-error";
import {
  decodeCommittees,
  ICommittee,
} from "../queries/get-committee-by-id.query";
import { taskEither } from "fp-ts";
import { Plan } from "../utils/enums/plan.enum";

export const getCommitteeByStripeAccountId =
  (committeeTable: string) =>
  (dynamoDB: DynamoDB) =>
  async (accountId: string): Promise<any> => {};

export const getCommitteeByStripeAccountAndDecode =
  (committeeTable: string) =>
  (dynamoDB: DynamoDB) =>
  (stripeAccount: string): TaskEither<ApplicationError, ICommittee> =>
    pipe(
      taskEither.tryCatch(
        () =>
          getCommitteesByStripeAccount(committeeTable)(dynamoDB)(stripeAccount),
        (err) => new ApplicationError("Get committee request failed", err)
      ),
      taskEither.chain(decodeCommittees),
      taskEither.chain(findOne)
    );

export const getCommitteesByStripeAccount =
  (committeesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (stripeAccount: string): Promise<any> => {
    const res = await dynamoDB
      .scan({
        TableName: committeesTableName,
        FilterExpression: "stripeAccount = :stripeAccount",
        ExpressionAttributeValues: {
          ":stripeAccount": { S: stripeAccount },
        },
      })
      .promise();
    return res.Items.map((item) => DynamoDB.Converter.unmarshall(item));
  };

export const findOne = <a>(list: a[]): TaskEither<ApplicationError, a> => {
  switch (list.length) {
    case 0:
      return taskEither.left(new ApplicationError("Does not exist", {}));
    case 1:
      return taskEither.right(list[1]);
    default:
      return taskEither.left(new ApplicationError("Duplicates found", {}));
  }
};
