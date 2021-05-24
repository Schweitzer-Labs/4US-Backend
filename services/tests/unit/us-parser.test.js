import { expect } from "chai";
import { parseUA } from "../../src/utils/parse-ua";

const validUa =
  "Mozilla/5.0 (iPad; CPU OS 11_0 like Mac OS X) AppleWebKit/604.1.34 (KHTML, like Gecko) Version/11.0 Mobile/15A5341f Safari/604.1";

describe("Tests User Agent Parser", function () {
  it("Parses a user agent string", async () => {
    const result = parseUA(validUa);
    expect(result.browser.name).to.equal("Mobile Safari");
  });
});
