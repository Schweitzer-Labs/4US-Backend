import { expect } from "chai";
import { genTxnId, randomString } from "../../src/utils/gen-txn-id.utils";

/**
 * @param k — size of alphabet
 * @param l — length of identifiers
 * @param n — number of generated identifiers
 */
function probCollision(k, l, n) {
  // d — total number of possible identifiers
  const d = Math.pow(k, l);
  return Math.pow((d - 1) / d, (n * (n - 1)) / 2);
}

describe("Generate Transaction ID Function", function () {
  it("Has two parts delimited by a '-'", () => {
    const id = genTxnId();
    const sections = id.split("-");
    expect(sections.length).to.equal(2);
  });
  it("Starts with a number", () => {
    const id = genTxnId();
    const sections = id.split("-");
    const date = parseInt(sections[0]);
    expect(date).to.be.a("number");
  });
  it("Ends with 6 character alphanumeric string", () => {
    const id = genTxnId();
    const sections = id.split("-");
    const str = sections[1];
    expect(str.length).to.equal(6);
  });

  it("Has a low collision rate", () => {
    const limit = 5000;
    let collision = false;
    let col = [];

    let i = 0;
    while (i < limit) {
      const id = randomString(6);
      if (col.includes(id)) {
        console.log(`duplicate discovered at ${i}`);
        collision = true;
        i = limit;
      } else {
        col.push(id);
        i++;
      }
    }
    expect(collision).to.equal(false);
  });
});
