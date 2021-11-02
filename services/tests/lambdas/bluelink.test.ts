import { expect } from "chai";
import bluelink from "../../src/bluelink.lambda";
import { EmploymentStatus } from "../../src/utils/enums/employment-status";
import * as faker from "faker";
import { genCommittee } from "../utils/gen-committee.util";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import { DynamoDB } from "aws-sdk";
import { deleteCommittee } from "../../src/utils/model/delete-committee.utils";
import { EntityType } from "../../src/utils/enums/entity-type.enum";
import { IOwner } from "../../src/queries/search-transactions.decoder";

dotenv.config();

AWS.config.apiVersions = {
    dynamodb: "2012-08-10",
};
const dynamoDB = new DynamoDB();

const committeesTableName: any = process.env.COMMITTEES_DDB_TABLE_NAME;

const committee = genCommittee({
    state: "ny",
    scope: "local",
    party: "republican",
    race: "general",
    district: "",
    county: "saratoga",
    officeType: "supervisor",
    ruleVersion: "nyboe-2020",
});

const genEvent = (payload: object) => {
    return {
        body: JSON.stringify(payload),
    };
};

describe("Bluelink webhook", function () {
    before(async () => {
        // await putCommittee(committeesTableName)(dynamoDB)(committee);
        // await sleep(1000);
    });

    it("Returns a 401 on auth failure", async () => {
        const testEvent = genEvent({

        });

        const res = await bluelink(testEvent);
        const body = JSON.parse(res.body);

        expect(res.statusCode).to.equal(401);
        expect(body.message).to.equal("success");
    });

    it("Returns a 400 on schema failure", async () => {
        const testEvent = genEvent({

        });

        const res = await bluelink(testEvent);
        const body = JSON.parse(res.body);

        expect(res.statusCode).to.equal(400);
        expect(body.message).to.equal("success");
    });

    it("Returns a 200 on schema success", async () => {
        const testEvent = genEvent({});

        const res = await bluelink(testEvent);
        const body = JSON.parse(res.body);

        expect(res.statusCode).to.equal(200);
        expect(body.message).to.equal("success");
    });


    after(async () => {
        // await deleteCommittee(committeesTableName)(dynamoDB)(committee);
    });
});
