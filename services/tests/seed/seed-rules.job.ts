import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import * as dotenv from "dotenv";
import { IRule } from "../../src/queries/get-rule.decoder";
import { rulesData } from "./rules.data";
import { ruleToRuleCode } from "../../src/utils/model/gen-rule-code.utils";

dotenv.config();

const rulesTableName = process.env.RULES_DDB_TABLE_NAME;

const run =
  (rulesTableName: string) =>
  (dynamoDB: DynamoDB) =>
  (sequence: number) =>
  async (data: any[]) => {
    const list = data.slice(25 * sequence - 25, 25 * sequence);
    const items = list.map((r) => {
      const rule: IRule = {
        code: "",
        state: r.state,
        scope: r.scope,
        party: r.party,
        race: r.race,
        district: r.district,
        county: r.county,
        officeType: r.office_type,
        ruleVersion: r.rule_version,
        entityType: r.contributor_type,
        aggregateDuration: r.aggregate_duration,
        fields: r.fields.split(","),
      };
      const code = ruleToRuleCode(rule);
      console.log(code);
      const marshalledRule = DynamoDB.Converter.marshall({
        ...rule,
        code,
      });

      return {
        PutRequest: {
          Item: marshalledRule,
        },
      };
    });

    return await dynamoDB
      .batchWriteItem({
        RequestItems: {
          [rulesTableName]: items,
        },
      })
      .promise();
  };

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

run(rulesTableName)(dynamoDB)(1)(rulesData)
  .then(console.log)
  .catch(console.log);
run(rulesTableName)(dynamoDB)(2)(rulesData)
  .then(console.log)
  .catch(console.log);
run(rulesTableName)(dynamoDB)(3)(rulesData)
  .then(console.log)
  .catch(console.log);
run(rulesTableName)(dynamoDB)(4)(rulesData)
  .then(console.log)
  .catch(console.log);
