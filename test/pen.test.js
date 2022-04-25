import {expect} from "chai"
import {Pen} from "../build/index.js"

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
})
