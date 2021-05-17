import { Field, ID, ObjectType } from "type-graphql";

import { registerEnumType } from "type-graphql";

enum Direction {
  IN = "in",
  OUT = "out",
}

registerEnumType(Direction, {
  name: "Direction", // this one is mandatory
  description: "Direction of payment", // this one is optional
});

@ObjectType()
export class Transaction {
  @Field((type) => ID)
  id: string;

  @Field()
  amount: number;

  @Field()
  direction: Direction;
}
