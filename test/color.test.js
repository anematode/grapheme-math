import { Color } from "../build/other/color.js"
import {expect} from "chai"

describe("Color", () => {
  describe("Color.fromObj", () => {
    it("Correctly parses various values", () => {
      expect(Color.fromObj(null)).to.deep.equal(Color.default())
      expect(Color.fromObj(0)).to.deep.equal(Color.default())

      expect(Color.fromObj("rgba(100,42,21,0.0393)")).to.deep.equal(new Color(100, 42, 21, 10))
      expect(Color.fromObj("rgb(100,42,21)")).to.deep.equal(new Color(100, 42, 21, 255))
      expect(Color.fromObj("rgb(-1, -1,  -1)")).to.deep.equal(new Color(0, 0, 0, 255))

      expect(Color.fromObj("red")).to.deep.equal(new Color(255, 0, 0, 255))
    })
  })
})
