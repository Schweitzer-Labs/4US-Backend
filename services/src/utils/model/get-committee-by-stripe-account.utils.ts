import { DynamoDB } from "aws-sdk";
import { TaskEither } from "fp-ts/TaskEither";
import { pipe } from "fp-ts/function";
import { taskEither } from "fp-ts";
import { ApplicationError } from "../application-error";
import { decodeCommittees } from "./get-committee-by-id.query";
import { StatusCodes } from "http-status-codes";
import { ICommittee } from "../../model/committee.type";

export const get_committee_by_stripe_account_and_decode =
  (committeeTable: string) =>
  (dynamoDB: DynamoDB) =>
  (stripeAccount: string): TaskEither<ApplicationError, ICommittee> =>
    pipe(
      taskEither.tryCatch(
        () =>
          getCommitteesByStripeAccount(committeeTable)(dynamoDB)(stripeAccount),
        (err) =>
          new ApplicationError(
            "Get committee request failed",
            err,
            StatusCodes.OK
          )
      ),
      taskEither.chain(decodeCommittees),
      taskEither.chain(findOne)
    );

export const getCommitteesByStripeAccount =
  (committeesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  async (stripeAccount: string): Promise<any> => {
    console.log("Get committees by stripe account called", stripeAccount);
    const res = await dynamoDB
      .scan({
        TableName: committeesTableName,
        FilterExpression: "stripeAccount = :stripeAccount",
        ExpressionAttributeValues: {
          ":stripeAccount": { S: stripeAccount },
        },
      })
      .promise();
    const committees = res.Items.map((item) =>
      DynamoDB.Converter.unmarshall(item)
    );

    console.log(
      "Get committees by stripe account res",
      JSON.stringify(committees)
    );

    return committees;
  };

export const findOne = (
  list: ICommittee[]
): TaskEither<ApplicationError, ICommittee> => {
  console.log(list);
  if (list.length > 0) {
    // @ToDo Address case with multiple committees with same stripe account.
    return taskEither.right(list[0]);
  } else {
    return taskEither.left(new ApplicationError("Does not exist", {}));
  }
};
