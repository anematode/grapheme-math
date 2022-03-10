import {parseString, ParserError} from "../src/ast/parse.js"
import { expect } from "chai"
import {expectMultipleCases} from "./test.js"

describe("parseString", () => {
  it("Correctly parses various expressions", () => {
    // TODO provide hard test cases, esp with weird ones like "and" and "or"
    const testCases = [
      ["x^2", `OperatorNode{name="^", children=List{VariableNode{name="x"}, ConstantNode{type=int, value=2}}}`],
      ["a+b+c", `OperatorNode{name="+", children=List{OperatorNode{name="+", children=List{VariableNode{name="a"}, VariableNode{name="b"}}}, VariableNode{name="c"}}}`], // left to right
      
    ]

    expectMultipleCases(parseString, testCases, "parseString")
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
