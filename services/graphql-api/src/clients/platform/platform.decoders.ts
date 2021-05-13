import * as t from "io-ts";

export enum Direction {
  IN = "in",
  OUT = "out",
}

const DirectionEnum = t.keyof({
  [Direction.IN]: null,
  [Direction.OUT]: null,
});

const PlatformTransactionRequired = t.type({
  firstName: t.string,
  lastName: t.string,
  occupation: t.string,
  direction: DirectionEnum,
});

const PlatformTransactionOptional = t.partial({
  middleName: t.string,
  employer: t.string,
  occupation: t.string,
  stripePaymentIntentId: t.string,
});

export const PlatformTransaction = t.intersection([
  PlatformTransactionRequired,
  PlatformTransactionOptional,
]);

export type PlatformTransaction = t.TypeOf<typeof PlatformTransaction>;
