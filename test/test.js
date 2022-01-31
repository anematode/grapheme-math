import { expect } from "chai"
import { roundUp } from "../src/example.js"

describe('roundUp', function() {
  it('should round up', function() {
    expect(roundUp(0)).to.equal(5e-324)


  });
});
