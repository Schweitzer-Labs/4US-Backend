import { Field, ObjectType } from "type-graphql";

@ObjectType()
export class Report {
  @Field((type) => String)
  csvData: string;
}
