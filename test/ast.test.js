import {parseExpression, ParseExpressionError, Complex, compileNode, RealInterval} from "../build/index.js"
import { expect } from "chai"
import {expectMultipleCases} from "./test.js"

describe("ast", () => {
  describe("parseExpression", () => {
    it("Correctly parses various expressions", () => {
      // TODO provide hard test cases, esp with weird ones like "and" and "or"
      const testCases = [
        ["x^2", `OperatorNode{name="^", children=List{VariableNode{name="x"}, ConstantNode{type=int, value=2}}}`],
        ["a+b+c", `OperatorNode{name="+", children=List{OperatorNode{name="+", children=List{VariableNode{name="a"}, VariableNode{name="b"}}}, VariableNode{name="c"}}}`], // left to right
      ]

      expectMultipleCases(c => parseExpression(c).prettyPrint(), testCases, "parseExpression")
    })

    it("Correctly throws on malformed expressions", () => {
      const testCases = [
        "x^", "", "   \t", "^", "^^", "x^^2", "f((2)(3))",
        "()", "$", "f ()", "x y", "2(3)", "(2, 1)", "(x+y, 2)", "f(x+y, 3,)",
        "sin^2(x)", "x << 3", "((3+f(4)+5)",
        "(2)3", '|', '||', '|x|', '||x||', '|x|^2'
      ]

      for (const testCase of testCases) {
        expect(() => parseExpression(testCase), `parseExpression attempted to parse: ${testCase}`).to.throw(ParseExpressionError)
      }
    })
  })

  describe("normal real evaluation", () => {
    it("Works for (x+3)^2", () => {
      let n = parseExpression("(x+3)^2").resolveTypes()

      expect(n.evaluate({ x: 2 })).to.eq(25)
      expect(n.evaluate({ x: -3 })).to.eq(0)
      expect(Number.isNaN(n.evaluate({ x: NaN }))).to.eq(true)
    })

    it("Works for (x+3)^-2", () => {
      let n = parseExpression("(x+3)^-2").resolveTypes()

      expect(n.evaluate({ x: 2 })).to.eq(0.04)
      expect(n.evaluate({ x: -3 })).to.eq(Infinity)
      expect(Number.isNaN(n.evaluate({ x: NaN }))).to.eq(true)
    })

    it("Works for x^2+y^2+z^2", () => {
      let n = parseExpression("x^2+y^2+z^2").resolveTypes()

      expect(n.evaluate({ x: 2, y: -2, z: -2 })).to.eq(12)

      expect(() => n.evaluate({ x: -3 })).to.throw()  // not defined in current scope
      expect(() => n.evaluate({ x: "cow", y: -2, z: -2 })).to.throw() // type checks
      expect(() => n.evaluate({ x: "cow", y: -2, z: -2 }, { typecheck: false })).to.not.throw()
    })

    it("Works for e^-x^2+e^-y^2", () => {
      let n = parseExpression("e^-x^2+e^-y^2").resolveTypes()

      expect(n.evaluate({ x: 2, y: 2 })).to.equal(0.03663127777746837)
      expect(n.evaluate({ x: -2, y: -2 })).to.equal(0.03663127777746837)
    })

    it("Works for cos(x *\\ty) (literal tab character)", () => {
      let n = parseExpression("cos(x *\ty)").resolveTypes()

      expect(n.evaluate({ x: 0, y: 0 })).to.equal(1)
      expect(n.evaluate({ x: 1, y: 1 })).to.equal(0.5403023058681398)
    })
  })

  describe("interval real evaluation", () => {
    describe("interval", () => {
      it("Works for x*x+y", () => {
        let n = parseExpression("x*x+y").resolveTypes({})

        expect(n.evaluate({ x: new RealInterval(0, 2), y: new RealInterval(0, 3) }, { mode: "fast_interval" }))
          .to.deep.equal(new RealInterval(0, 7, 0b1111))

        expect(n.evaluate({ x: new RealInterval(-2, 2), y: new RealInterval(0, 3) }, { mode: "fast_interval" }))
          .to.deep.equal(new RealInterval(-4, 7, 0b1111))

        expect(() => n.evaluate(2, { mode: "fast_interval" })).to.throw()
        expect(() => n.evaluate({ x: new RealInterval(-2, 2) }, { mode: "fast_interval" })).to.throw()
        expect(() => n.evaluate({ x: 2, y: 3 }, { mode: "fast_interval" })).to.throw()
      })
    })
  })

  describe("mathematical constants", () => {
    it("Works for pi and e", () => {
      let n = parseExpression("pi+e").resolveTypes()

      expect(n.evaluate({})).to.equal(5.859874482048838)
      expect(n.evaluate()).to.equal(5.859874482048838)
    })

    it("Works for i", () => {
      let n = parseExpression("i * 2").resolveTypes()

      expect(n.evaluate()).to.deep.equal(new Complex(0, 2))
    })
  })

  describe("compilation", () => {
    describe("normal", () => {
      it("Defaults to normal, with typechecks, with a single target and using scope input", () => {
        let n = compileNode("x * x + y")

        expect(n.targets[0].evaluate).to.be.a("function")
        expect(n.targets.length).to.equal(1)

        expect(n.targets[0].evaluate({ x: 2, y: 5 })).to.equal(9)
        expect(Number.isNaN(n.targets[0].evaluate({ x: 2, y: NaN }))).to.equal(true)
        expect(() => n.targets[0].evaluate({ x: "chicken", y: 5 })).to.throw()
        expect(() => n.targets[0].evaluate({ x: 3, y: "chicken" })).to.throw()
        expect(() => n.targets[0].evaluate({ x: 0 })).to.throw()
        expect(() => n.targets[0].evaluate({ y: 9 })).to.throw()
      })

      it("Allows a custom input format", () => {
        let e = compileNode("x * x + y", {
          inputFormat: [ "x", "y" ]
        }).targets[0].evaluate

        expect(e(2, 3)).to.eq(7)

        e = compileNode("x * x + y", {
          inputFormat: [ "scope", "y" ]
        }).targets[0].evaluate

        expect(e({ x: 2 }, 3)).to.eq(7)

        expect(() => compileNode("x * x + y", {
          inputFormat: [ "y" ]
        })).to.throw()
      })
    })

    describe("interval", () => {
      it("works for x*x+y", () => {
        let n = compileNode("x * x + y", {
          inputFormat: [ "x", "y" ], mode: "fast_interval"
        })

        let e = n.targets[0].evaluate
        expect(e).to.be.a("function")
        expect(e(new RealInterval(0, 2), new RealInterval(0, 3)))
          .to.deep.equal(new RealInterval(0, 7))
      })

      it("works for x/y", () => {
        let n = compileNode("x/y", {
          inputFormat: [ "x", "y" ], mode: "fast_interval"
        })

        let e = n.targets[0].evaluate
        expect(e).to.be.a("function")
        expect(e(new RealInterval(-2, 2), new RealInterval(1, 3)))
          .to.deep.equal(new RealInterval(-2, 2))
        expect(e(new RealInterval(-2, 2), new RealInterval(-1, 1)).info)
          .to.equal(0b0101)
      })
    })
  })
})
