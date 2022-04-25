import {expect} from "chai"
import {Pen} from "../build/other/pen.js"

describe("Pen", () => {
  describe("Pen.compose", () => {
    it("Works for various inputs", () => {
      expect(Pen.compose({ thickness: 2 })).to.deep.equal(Pen.create({ thickness: 2 }))
      expect(Pen.compose({ thickness: 2, color: "red" }, { thickness: 4, color: "blue" }))
        .to.deep.equal(Pen.create({ thickness: 4, color: "blue" }))

      expect(Pen.compose({ thickness: 2, color: "red" }, { thickness: 4 }))
        .to.deep.equal(Pen.create({ thickness: 4, color: "red" }))
    })
  })

  describe("Pen enums are the same as before", () => {
    const ENDCAP_TYPES = {
      butt: 0,
      round: 1,
      square: 2
    }

    // Enum for join types
    const JOIN_TYPES = {
      bevel: 0,
      miter: 2,
      round: 1,
      dynamic: 3
    }

    expect(Pen.ENDCAP_TYPES).to.deep.equal(ENDCAP_TYPES)
    expect(Pen.JOIN_TYPES).to.deep.equal(JOIN_TYPES)
  })
})
