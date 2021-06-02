import { expect } from "chai";
import { InstantIdResponse } from "../../src/clients/lexis-nexis/lexis-nexis.decoder";
import { fortyInstantIdConsumer } from "../lexis-nexis/forty.instant-id-consumer";
import { isLeft } from "fp-ts/Either";
import { twentyInstantIdConsumer } from "../lexis-nexis/twenty.instant-id-consumer";
import { zeroInstantIdConsumer } from "../lexis-nexis/zero.instant-id-consumer";

describe("Lexis Nexis Decoders", function () {
  describe("Instant ID Decoder", () => {
    it("Decodes a 40 CVI identity", () => {
      const res = InstantIdResponse.decode(fortyInstantIdConsumer);

      if (isLeft(res)) {
        throw Error("test failed");
      }

      expect(
        res.right.InstantIDResponseEx.response.Result.ComprehensiveVerification
          .ComprehensiveVerificationIndex
      ).to.equal(40);

      expect(
        res.right.InstantIDResponseEx.response.Result.UniqueId !== "0"
      ).to.equal(true);
    });
    it("Decodes a 20 CVI identity", () => {
      const res = InstantIdResponse.decode(twentyInstantIdConsumer);

      if (isLeft(res)) {
        throw Error("test failed");
      }

      expect(
        res.right.InstantIDResponseEx.response.Result.ComprehensiveVerification
          .ComprehensiveVerificationIndex
      ).to.equal(20);

      expect(
        res.right.InstantIDResponseEx.response.Result.UniqueId !== "0"
      ).to.equal(true);
    });
    it("Decodes a 0 CVI identity", () => {
      const res = InstantIdResponse.decode(zeroInstantIdConsumer);

      if (isLeft(res)) {
        throw Error("test failed");
      }

      expect(
        res.right.InstantIDResponseEx.response.Result.ComprehensiveVerification
          .ComprehensiveVerificationIndex
      ).to.equal(0);

      expect(
        res.right.InstantIDResponseEx.response.Result.UniqueId === "0"
      ).to.equal(true);
    });
  });
});
