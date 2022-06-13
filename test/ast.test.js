import {parseString, ParserError} from "../build/ast/parse.js"
import { expect } from "chai"
import {expectMultipleCases} from "./test.js"

describe("ast", () => {
  describe("parseString", () => {
    it("Correctly parses various expressions", () => {
      // TODO provide hard test cases, esp with weird ones like "and" and "or"
      const testCases = [
        ["x^2", `OperatorNode{name="^", children=List{VariableNode{name="x"}, ConstantNode{type=int, value=2}}}`],
        ["a+b+c", `OperatorNode{name="+", children=List{OperatorNode{name="+", children=List{VariableNode{name="a"}, VariableNode{name="b"}}}, VariableNode{name="c"}}}`], // left to right
      ]

      expectMultipleCases(c => parseString(c).prettyPrint(), testCases, "parseString")
    })

    it("Correctly throws on malformed expressions", () => {
      const testCases = [
        "x^", "", "   \t", "^", "^^", "x^^2", "f((2)(3))",
        "()", "$", "f ()", "x y", "2(3)", "(2, 1)", "(x+y, 2)", "f(x+y, 3,)",
        "sin^2(x)", "x << 3", "((3+f(4)+5)",
        "(2)3"
      ]

      for (const testCase of testCases) {
        expect(() => parseString(testCase), `parseString attempted to parse: ${testCase}`).to.throw(ParserError)
      }
    })
  })

  describe("real evaluation", () => {
    it("Works for (x+3)^2", () => {
      let n = parseString("(x+3)^2").resolveTypes()

      expect(n.evaluate({ x: 2 })).to.eq(25)
      expect(n.evaluate({ x: -3 })).to.eq(0)
      expect(Number.isNaN(n.evaluate({ x: NaN }))).to.eq(true)
    })
  })

  describe("compile", () => {
    describe("normal", () => {

    })
  })
})
