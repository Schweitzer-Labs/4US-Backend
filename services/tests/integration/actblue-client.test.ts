import { expect } from "chai";
import * as dotenv from "dotenv";
import * as AWS from "aws-sdk";
import {
  ActBlueCSVType,
  IActBlueAPICredentials,
  IActBluePaidContribution,
} from "../../src/clients/actblue/actblue.decoders";
import { pipe } from "fp-ts/function";
import { task, taskEither } from "fp-ts";
import { nMonthsAgo, now } from "../../src/utils/time.utils";
import { isLeft } from "fp-ts/Either";
import { sleep } from "../../src/utils/sleep.utils";
import {
  actBlueCSVMetadataToTypedData,
  getActBlueCSVMetadata,
} from "../../src/clients/actblue/actblue.client";

dotenv.config();

const secret = process.env.ACTBLUE_CLIENT_SECRET;
const uuid = process.env.ACTBLUE_CLIENT_UUID;

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

let csvId;

const creds: IActBlueAPICredentials = {
  clientSecret: secret,
  clientUUID: uuid,
};

describe("ActBlue Client", function () {
  describe("CSV generation", function () {
    it("Request CSV generation and retrieves a CSV Metadata", async () => {
      const rn = now();
      const sixMAgo = nMonthsAgo(6)(rn);

      const eitherCSVId = await getActBlueCSVMetadata(
        ActBlueCSVType.PaidContributions
      )(creds)(sixMAgo)(rn)();
      if (isLeft(eitherCSVId)) {
        throw Error("request failed");
      }

      expect(eitherCSVId.right.csvId).to.be.a("string");
    });
  });

  describe("CSV request and decoding", function () {
    before(async () => {
      const rn = now();
      const sixMAgo = nMonthsAgo(6)(rn);

      const eitherCSVId = await getActBlueCSVMetadata(
        ActBlueCSVType.PaidContributions
      )(creds)(sixMAgo)(rn)();
      if (isLeft(eitherCSVId)) {
        throw Error("request failed");
      }

      await sleep(6000);

      csvId = eitherCSVId.right.csvId;
    });

    it("Retrieves typed data from a CSV Id", async () => {
      const res: IActBluePaidContribution[] = await pipe(
        actBlueCSVMetadataToTypedData(csvId)(creds),
        taskEither.fold(
          () => task.of([]),
          (res) => task.of(res)
        )
      )();

      expect(res.length > 0).to.equal(true);
    });
  });
});
