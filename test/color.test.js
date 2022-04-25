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
      expect(Color.fromObj("   blue ")).to.deep.equal(new Color(0, 0, 255, 255))

      expect(Color.fromObj("hsl(195, 100%, 50%)")).to.deep.equal(new Color(0, 191, 255, 255))
      expect(Color.fromObj("hsla(195, 100%, 50%, 0.3)")).to.deep.equal(new Color(0, 191, 255, Math.round(0.3 * 255)))

      expect(Color.fromObj("#02ff04")).to.deep.equal(new Color(2, 255, 4, 255))
      expect(Color.fromObj("#02ff04aa")).to.deep.equal(new Color(2, 255, 4, 170))
    })
  })
})
