import {parseString, ParserError, Complex, compileNode} from "../build/index.js"
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

  describe("normal real evaluation", () => {
    it("Works for (x+3)^2", () => {
      let n = parseString("(x+3)^2").resolveTypes()

      expect(n.evaluate({ x: 2 })).to.eq(25)
      expect(n.evaluate({ x: -3 })).to.eq(0)
      expect(Number.isNaN(n.evaluate({ x: NaN }))).to.eq(true)
    })

    it("Works for (x+3)^-2", () => {
      let n = parseString("(x+3)^-2").resolveTypes()

      expect(n.evaluate({ x: 2 })).to.eq(0.04)
      expect(n.evaluate({ x: -3 })).to.eq(Infinity)
      expect(Number.isNaN(n.evaluate({ x: NaN }))).to.eq(true)
    })

    it("Works for x^2+y^2+z^2", () => {
      let n = parseString("x^2+y^2+z^2").resolveTypes()

      expect(n.evaluate({ x: 2, y: -2, z: -2 })).to.eq(12)

      expect(() => n.evaluate({ x: -3 })).to.throw()  // not defined in current scope
      expect(() => n.evaluate({ x: "cow", y: -2, z: -2 })).to.throw() // type checks
      expect(() => n.evaluate({ x: "cow", y: -2, z: -2 }, { typecheck: false })).to.not.throw()
    })

    it("Works for e^-x^2+e^-y^2", () => {
      let n = parseString("e^-x^2+e^-y^2").resolveTypes()

      expect(n.evaluate({ x: 2, y: 2 })).to.equal(0.03663127777746837)
      expect(n.evaluate({ x: -2, y: -2 })).to.equal(0.03663127777746837)
    })

    it("Works for cos(x *\\ty) (literal tab character)", () => {
      let n = parseString("cos(x *\ty)").resolveTypes()

      expect(n.evaluate({ x: 0, y: 0 })).to.equal(1)
      expect(n.evaluate({ x: 1, y: 1 })).to.equal(0.5403023058681398)
    })
  })

  describe("mathematical constants", () => {
    it("Works for pi and e", () => {
      let n = parseString("pi+e").resolveTypes()

      expect(n.evaluate({})).to.equal(5.859874482048838)
      expect(n.evaluate()).to.equal(5.859874482048838)
    })

    it("Works for i", () => {
      let n = parseString("i * 2").resolveTypes()

      expect(n.evaluate()).to.deep.equal(new Complex(0, 2))
    })
  })

  describe("compilation", () => {
    describe("normal", () => {
      it("Defaults to normal, with typechecks, with a single target and using scope input", () => {
        let n = compileNode(parseString("x * x + y").resolveTypes())

        expect(n.targets[0].evaluate).to.be.a("function")
        expect(n.targets.length).to.equal(1)

        expect(n.targets[0].evaluate({ x: 2, y: 5 })).to.equal(9)
        expect(Number.isNaN(n.targets[0].evaluate({ x: 2, y: NaN }))).to.equal(true)
        expect(() => n.targets[0].evaluate({ x: "chicken", y: 5 })).to.throw()
        expect(() => n.targets[0].evaluate({ x: 3, y: "chicken" })).to.throw()
        expect(() => n.targets[0].evaluate({ x: 0 })).to.throw()
        expect(() => n.targets[0].evaluate({ y: 9 })).to.throw()
      })
    })
  })
})
