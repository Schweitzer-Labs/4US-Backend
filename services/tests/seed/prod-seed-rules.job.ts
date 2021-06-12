import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import * as dotenv from "dotenv";
import { IRule } from "../../src/queries/get-rule.decoder";
import { rulesData } from "./safford-rules.data";
import { ruleToRuleCode } from "../../src/utils/model/gen-rule-code.utils";
import { sleep } from "../../src/utils/sleep.utils";

dotenv.config();

AWS.config.region = "us-east-1";
const rulesTableName = "prod-4us-backend-Rules-1M2YB2SS4K4LZ";

let index = 1;
const run =
  (rulesTableName: string) => (dynamoDB: DynamoDB) => async (data: any[]) => {
    const length = data.length;
    const remaining = length % 25;
    const sequences = Math.ceil((length - remaining) / 25);

    for (let i = 1; i <= sequences + 1; i++) {
      const list = data.slice(25 * i - 25, 25 * i);
      const items = list.map((r) => {
        const rule: IRule = {
          code: "",
          state: r.state,
          scope: r.scope,
          party: r.party,
          race: r.race,
          district: r.district + "",
          county: r.county,
          officeType: r.officeType,
          ruleVersion: r.ruleVersion,
          entityType: r.contributor_type,
          limit: r.limit === "NULL" ? null : r.limit,
          aggregateDuration: r.aggregate_duration,
          fields: r.fields.split(","),
        };
        const code = ruleToRuleCode(rule);
        const marshalledRule = DynamoDB.Converter.marshall({
          ...rule,
          code,
        });

        console.log(index);

        index++;

        return {
          PutRequest: {
            Item: marshalledRule,
          },
        };
      });

      await dynamoDB
        .batchWriteItem({
          RequestItems: {
            [rulesTableName]: items,
          },
        })
        .promise()
        .then((data) => console.log(`Batch ${i} written`))
        .catch(console.log)
        .finally(console.log);

      await sleep(100);
    }
  };

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

run(rulesTableName)(dynamoDB)(rulesData);
// run(rulesTableName)(dynamoDB)(1)(rulesData);
// run(rulesTableName)(dynamoDB)(2)(rulesData);
// run(rulesTableName)(dynamoDB)(3)(rulesData);
// run(rulesTableName)(dynamoDB)(4)(rulesData);
// run(rulesTableName)(dynamoDB)(5)(rulesData);
