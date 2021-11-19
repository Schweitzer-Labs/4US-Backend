import { expect } from "chai";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { flow, pipe } from "fp-ts/function";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

interface IToDoArgs {
  first: string;
  second: string;
  third: string;
}

// (a -> b -> c -> d) -> (d -> e) -> (a -> b -> c -> e)

type Abcd = (a: string) => (b: string) => (c: string) => IToDoArgs;

type de = (d: string) => (e: number) => string;

const toDoArgs =
  (first: string) =>
  (second: string) =>
  (third: string): IToDoArgs => ({
    first,
    second,
    third,
  });

const toDo =
  ({ first, second, third }: IToDoArgs) =>
  (num: number): string =>
    `${first} ${second} ${third} -> ${num.toString()}`;

const fn = flow((str) => toDoArgs());

describe("Describe here", function () {
  it("Tests something", async () => {
    const res = flow;

    expect(true).to.equal(false);
  });
});
