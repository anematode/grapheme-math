import {
  addMantissas,
  BigFloat,
  compareMantissas,
  getTrailingInfo,
  getTrailingInfo2,
  leftShiftMantissa,
  multiplyMantissas,
  neededWordsForPrecision,
  prettyPrintMantissa,
  rightShiftMantissa,
  roundMantissaToPrecision,
  subtractMantissas,
  verifyMantissa
} from '../src/arb/bigfloat.js'

import { ROUNDING_MODE, roundingModeToString } from "../src/rounding_modes.js"
import { expect } from "chai"
import {deepEquals, rightZeroPad} from "../grapheme_shared.js"
import {
  addMantissas as referenceAddMantissas,
  subtractMantissas as referenceSubtractMantissas,
  multiplyMantissas as referenceMultiplyMantissas
} from "../src/arb/reference.js"
import {difficultMantissas, typicalMantissas} from "./test_common.js"
import {expectMultipleCases} from "./test.js"

const BF = BigFloat
const RM = ROUNDING_MODE

function prettyPrintArg (arg, smart=true /* for rounding mode*/) {
  if (arg instanceof Int32Array) {
    return prettyPrintMantissa(arg, '\x1b[32m')
  } else if (typeof arg === "number") {
    if (Object.is(arg, -0)) {
      return "-0"
    } else if (smart && Object.values(RM).includes(arg)) {
      return roundingModeToString(arg)
    } else {
      return arg + ''
    }
  } else {
    return arg + ''
  }
}

// Passed: array of arguments, size of target mantissa, expected value of target mantissa, and expected returned shift
function testMantissaCase (func, args, argNames, expectedTarget, expectedReturn) {
  verifyMantissa(expectedTarget)

  // Replace target argument with empty array of corresponding length
  let i = argNames.indexOf("target")

  let target = new Int32Array(args[i]) // give target size
  args[i] = target

  // So that they have the same length
  let typedExpectedTarget = new Int32Array(target.length)
  for (let i = 0; i < target.length; ++i) typedExpectedTarget[i] = expectedTarget[i]

  expectedTarget = typedExpectedTarget

  // Fill array with junk data, in case the array isn't cleared correctly
  target.fill(0x2BADBEEF)

  const originalTarget = new Int32Array(target)

  // Allow normal arrays to be used, for brevity
  args = args.map(a => Array.isArray(a) ? new Int32Array(a) : a)

  let ret = func(...args)

  function formatArgs (smart=true) {
    let out = ""
    for (let _i = 0; _i < argNames.length; ++_i) {
      out += `\n\x1b[32m${argNames[_i]}\x1b[0m: ${prettyPrintArg((i === _i) ? originalTarget : args[_i], smart)}`
    }
    return out
  }

  let wrongTarget = !deepEquals(target, expectedTarget)
  let wrongRet = !deepEquals(ret, expectedReturn)
  if (wrongTarget || wrongRet) {
    let toReproduce = `let target = ${prettyPrintArg(originalTarget)};
console.log("returned: ", GMath.${func.name}(${args.map((a, i) => argNames[i] === "target" ? "target" : prettyPrintArg(a, false)).join(', ')}));
console.log("target: ", GMath.prettyPrintMantissa(target, ''));`
    if (wrongTarget)
      throw new Error(`Incorrect result while testing function ${func.name}. Arguments are as follows: ${formatArgs()}\nExpected target mantissa: ${prettyPrintArg(expectedTarget)}\nActual mantissa:          ${prettyPrintMantissa(target, '\u001b[31m')}\n\nTo reproduce:\n${toReproduce}\n`)
    if (wrongRet)
      throw new Error(`Incorrect result while testing function ${func.name}. Arguments are as follows: ${formatArgs()}\nExpected return: ${prettyPrintArg(expectedReturn)}\nActual return: ${prettyPrintArg(ret)}\n\nTo reproduce:\n${toReproduce}\n`)
  }
}

describe("roundMantissaToPrecision", () => {
  const argNames = ["mant", "prec", "target", "round", "trailing", "trailingInfo"]

  it('fills in unused words with 0', () => {
    testMantissaCase(roundMantissaToPrecision, [ [ 0x1fffffff, 0 ], 20, 200 /* excess target length */, RM.NEAREST, 0, 0 ], argNames, [ 0x20000000, 0 ], 0)
  })

  // TODO test cases... if there's a bug in here, I won't find it for a while...
})

describe("addMantissas", () => {
  it("should behave identically to the reference implementation", () => {
    let argNames = ["mant1", "mant1Len", "mant2", "mant2Len", "mant2Shift", "prec", "target", "targetLen", "round"]

    let cases = 0
    let startTime = Date.now()

    // About 102 million test cases, should be somewhat thorough in terms of carry and rounding checking
    for (let i = 0; i < difficultMantissas.length; ++i) {
      const m1 = difficultMantissas[i]
      for (const m2 of difficultMantissas) {
        for (let shift = 0; shift < 5; ++shift) {
          for (let targetSize = 1; targetSize < 5; ++targetSize) {
            for (let precision of [30, 53, 59, 60, 120]) {
              for (let roundingMode of [0, 1, 2, 5]) {
                let target = new Int32Array(Math.max(neededWordsForPrecision(precision), targetSize))
                const args = [m1, m1.length, m2, m2.length, shift, precision, target, target.length, roundingMode]
                let ret = referenceAddMantissas(...args)

                testMantissaCase(addMantissas, args, argNames, target, ret)
                cases++
              }
            }
          }
        }
      }

      (!(i % 10)) ? console.log(`Progress: ${(i / difficultMantissas.length * 100).toPrecision(4)}% complete`) : 0
    }

    let endTime = Date.now()
    console.log(`Completed ${cases} test cases for addMantissas, comparing to referenceAddMantissas, in ${(endTime - startTime) / 1000} seconds.`)
  })
})

describe("subtractMantissas", () => {
  it("should behave identically to the reference implementation", () => {
    let argNames = ["mant1", "mant2", "mant2Shift", "prec", "target", "round"]

    let cases = 0
    let startTime = Date.now()

    // About 102 million test cases, should be somewhat thorough in terms of carry and rounding checking
    for (let i = 0; i < difficultMantissas.length; ++i) {
      const m1 = difficultMantissas[i]
      for (const m2 of difficultMantissas) {
        for (let shift = 0; shift < 5; ++shift) {
          // Eliminate invalid cases
          if (shift === 0) {
            let cmp = compareMantissas(m1, m2)
            if (cmp !== 1) continue
          }

          for (let targetSize = 0; targetSize < 5; ++targetSize) {
            for (let precision of [30, 53, 59, 60, 120]) {
              for (let roundingMode of [0, 1, 2, 5]) {
                let target = new Int32Array(neededWordsForPrecision(precision))
                let ret = referenceSubtractMantissas(m1, m2, shift, precision, target, roundingMode)

                testMantissaCase(subtractMantissas, [m1, m2, shift, precision, target.length, roundingMode], argNames, target, ret)
                cases++
              }
            }
          }
        }
      }

      (!(i % 10)) ? console.log(`Progress: ${(i / difficultMantissas.length * 100).toPrecision(4)}% complete`) : 0
    }

    let endTime = Date.now()
    console.log(`Completed ${cases} test cases for subtractMantissas, comparing to referenceSubtractMantissas, in ${(endTime - startTime) / 1000} seconds.`)
  })
})

describe("multiplyMantissas", () => {
  it("should behave identically to the reference implementation", () => {
    let argNames = ["mant1", "mant2", "prec", "target", "round"]

    let cases = 0
    let startTime = Date.now()

    let testMantissas = [ ...typicalMantissas, ...difficultMantissas ]


      for (let i = 0; i < testMantissas.length; ++i) {
        const m1 = testMantissas[i]
        for (const m2 of testMantissas) {
          for (let targetSize = 0; targetSize < 5; ++targetSize) {
            for (let precision of [30, 53, 59, 60, 120]) {
              for (let roundingMode of [0, 1, 2, 5]) {
                let target = new Int32Array(neededWordsForPrecision(precision))
                let ret = referenceMultiplyMantissas(m1, m2, precision, target, roundingMode)

                testMantissaCase(multiplyMantissas, [m1, m2, precision, target.length, roundingMode], argNames, target, ret)
                cases++
              }
            }
          }
        }

        (!(i % 10)) ? console.log(`Progress: ${(i / testMantissas.length * 100).toPrecision(4)}% complete`) : 0
    }

    console.log(globalThis.cow)

    let endTime = Date.now()
    console.log(`Completed ${cases} test cases for multiplyMantissas, comparing to referenceMultiplyMantissas, in ${(endTime - startTime) / 1000} seconds.`)
  })
})

describe("getTrailingInfo", () => {
  it("should return correct results", () => {
    let cases = [
      [[[0, 0x20000000], 1], 2],
      [[[0, 0x20000000, 0, 1], 1], 3],
      [[[0, 0x20000000, 0, 0], 1], 2],
      [[[0, 0x1fffffff, 0, 0], 1], 1],
      [[[0, 0x3fffffff, 0, 0], 1], 3],
      [[[0, 0, 0x3fffffff, 0], 1], 1],
      [[[0, 0, 0, 0], 1], 0],
      [[[0, 0, 0, 1], -1], 1]
    ]

    expectMultipleCases(getTrailingInfo, cases)
  })
})

describe("getTrailingInfo2", () => {
  it("should return correct results", () => {
    let cases = [
      [[[0, 0x20000000], 1], 2],
      [[[0, 0x20000000, 0, 1], 1], 3],
      [[[0, 0x20000000, 0, -1], 1], 1],
      [[[0, 0x20000000, 0, 0], 1], 2],
      [[[0, 0x1fffffff, 0, 0], 1], 1],
      [[[0, 0x3fffffff, 0, 0], 1], 3],
      [[[0, 0, 0x3fffffff, 0], 1], 1],
      [[[0, 0, 0, -1], 1], -1],
      [[[0, 0, 0, 0], 1], 0],
      [[[0, 0, 0, -0x3fffffff], 2], -1],
      [[[0, 0, -0x3fffffff, 0], 2], -3],
      [[[0, -0x20000000], 1], -2],
      [[[0, -0x20000000, 0, 1], 1], -1],
      [[[0, -0x20000000, 0, -1], 1], -3],
      [[[0, -0x20000000, 0, 0], 1], -2],
      [[[0, -0x1fffffff, 0, 0], 1], -1],
      [[[0, 0, 0, 1], -1], 1],
      [[[0, 0, 0, -1], -1], -1]
    ]

    expectMultipleCases(getTrailingInfo2, cases)
  })
})

// TODO: write tests for canRoundMantissa

/*describe("DeltaFloat", () => {
  const DF = DeltaFloat

  it('should convert correctly to and from numbers', () => {
    function testNum (n) {
      expect(Object.is(DF.fromNumber(n).toNumber(), n), `Result on ${n}`)
    }

    ;[0, NaN, Infinity, 1, 2, 1.5, 0.5, 0.75, 0.875].forEach(testNum)
  })
})*/
