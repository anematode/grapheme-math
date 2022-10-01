import {ROUNDING_MODE, roundingModeToString} from "../build/other/rounding_modes.js"
import {
    cartesianProduct, cartesianProductGen,
    difficultMantissas,
    PATHOLOGICAL_NUMBERS,
    ROUNDING_MODES, STRICT_ROUNDING_MODES,
    TYPICAL_NUMBERS, typicalMantissas,
    ALL_NUMBERS, RANDOM_BIGINTS,
    RANDOM_BIGFLOATS
} from "./test_common.js"
import {
    roundMantissaToPrecision,
    addMantissas,
    subtractMantissas,
    multiplyMantissas,
    neededWordsForPrecision,
    compareMantissas,
    BigFloat,
    prettyPrintMantissa,
    validateMantissa
} from "../build/arb/bigfloat.js"
import {expectMultipleCases} from "./test.js"
import {deepEquals} from "../build/utils.js"
import {
    addMantissas as referenceAddMantissas,
    subtractMantissas as referenceSubtractMantissas,
    multiplyMantissas as referenceMultiplyMantissas
} from "../build/arb/reference.js";
import {isDenormal} from "../build/fp/manip.js";

const BF = BigFloat
const RM = ROUNDING_MODE

function prettyPrintArg(arg, name, smart = true /* for rounding mode*/) {
    if (arg instanceof Int32Array) {
        return prettyPrintMantissa(arg, '\x1b[32m')
    } else if (typeof arg === "number") {
        if (Object.is(arg, -0)) {
            return "-0"
        } else if (name === "rm" && smart && Object.values(RM).includes(arg)) {
            return roundingModeToString(arg)
        } else {
            return arg + ''
        }
    } else {
        return arg + ''
    }
}

// Passed: array of arguments, size of target mantissa, expected value of target mantissa, and expected returned shift
// noCheck prevents checking whether the mantissa is correct
function testMantissaCase(func, args, argNames, expectedTarget, expectedReturn, noCheck=false) {

    // Replace target argument with empty array of corresponding length
    let i = argNames.indexOf("target")
    if (i === -1) i = argNames.indexOf("t")

    let target = new Int32Array(args[i]) // give target size
    args[i] = target

    // So that they have the same length
    let typedExpectedTarget = new Int32Array(target.length)
    for (let i = 0; i < target.length; ++i) typedExpectedTarget[i] = expectedTarget[i]

    expectedTarget = typedExpectedTarget
    if (!noCheck) validateMantissa(expectedTarget)

    // Fill array with junk data, in case the array isn't cleared correctly
    target.fill(0x2BADBEEF)

    const originalTarget = new Int32Array(target)

    // Allow normal arrays to be used, for brevity
    args = args.map(a => Array.isArray(a) ? new Int32Array(a) : a)

    let ret = func(...args)

    function formatArgs(smart = true) {
        let out = ""
        for (let _i = 0; _i < argNames.length; ++_i) {
            out += `\n\x1b[32m${argNames[_i]}\x1b[0m: ${prettyPrintArg((i === _i) ? originalTarget : args[_i], argNames[_i], smart)}`
        }
        return out
    }

    let wrongTarget = !noCheck && !deepEquals(target, expectedTarget)
    let wrongRet = !deepEquals(ret, expectedReturn)
    if (wrongTarget || wrongRet) {
        let toReproduce = `let target = ${prettyPrintArg(originalTarget)};
console.log("returned: ", Grapheme.${func.name}(${args.map((a, i) => ["target", "t"].includes(argNames[i]) ? "target" : prettyPrintArg(a, false)).join(', ')}));
console.log("target: ", Grapheme.prettyPrintMantissa(target, ''));`

        if (wrongTarget)
            throw new Error(`Incorrect result while testing function ${func.name}. Arguments are as follows: ${formatArgs()}\nExpected target mantissa: ${prettyPrintArg(expectedTarget)}\nActual mantissa:          ${prettyPrintMantissa(target, '\u001b[31m')}\n\nTo reproduce:\n${toReproduce}\n`)
        if (wrongRet)
            throw new Error(`Incorrect result while testing function ${func.name}. Arguments are as follows: ${formatArgs()}\nExpected return: ${prettyPrintArg(expectedReturn)}\nActual return: ${prettyPrintArg(ret)}\n\nTo reproduce:\n${toReproduce}\n`)
    }
}

// TODO: Whatever rounding
const TEST_RM = [ RM.TIES_ZERO, RM.TIES_AWAY, RM.TIES_ODD, RM.TIES_EVEN, RM.DOWN, RM.UP ]

describe("BigFloat", function () {
    this.timeout(0)  // big float stress testing is time consuming

    it("losslessly converts to and from numbers", () => {
        let f = BigFloat.new(53)
        let cases = cartesianProduct([PATHOLOGICAL_NUMBERS, ...TYPICAL_NUMBERS], ROUNDING_MODES).map(([n, rm]) => [[n, rm], n])

        expectMultipleCases((n, rm) =>
            f.setFromNumber(n, rm).toNumber(), cases)
    })

    it("losslessly converts to and from bigints", () => {
        let cases = RANDOM_BIGINTS.map(n => [[n], n])

        expectMultipleCases(n => BigFloat.fromNativeBigInt(n).toBigInt(), cases)
    })

    it("behaves like Math.fround when f32=true", () => {
        let f = BigFloat.new(53)
        let cases = cartesianProduct([...TYPICAL_NUMBERS, PATHOLOGICAL_NUMBERS], ROUNDING_MODES).map(([n, rm]) => [[n, rm], Math.fround(n)])

        expectMultipleCases((n, rm) => f.setFromNumber(n, rm).toNumber(ROUNDING_MODES.NEAREST, true), cases)
    })

    describe("roundMantissaToPrecision", () => {
        let args = ["m", "mLen", "t", "tLen", "prec", "rm", "trailing", "trailingMode"]
        it("satisfies difficult test cases", () => {
            let testCases = [
                [[0x3fffffff, 0x3fffffff, 0x3fffffff], 3, [1, 0, 0], 3, 50, RM.NEAREST, 0, 0, 1],
                [[0x3fffffff, 0x3ffffff0, 0x00000000], 3, [1, 0, 0], 3, 50, RM.NEAREST, 0, 0, 1],
                [[0x3fffffff, 0x3ffffff0, 0x00000000], 3, [1, 0, 0], 3, 55, RM.NEAREST, 0, 0, 1],
                [[0x3fffffff, 0x3ffffff0, 0x00000000], 3, [0x3fffffff, 0x3ffffff0, 0x00000000], 3, 56, RM.NEAREST, 0, 0, 0],
                [[0x3fffffff, 0x3ffffff0, 0x00000000], 3, [0x3fffffff, 0x3fffffe0, 0x00000000], 3, 55, RM.TIES_ODD, 0, 0, 0],
                [[0x3fffffff, 0x3ffffff0, 0x00000000], 3, [0x3fffffff, 0x3fffffe0, 0x00000000], 3, 55, RM.TIES_ZERO, 0, 0, 0],
                [[0x3fffffff, 0x3ffffff0, 0x00000000], 3, [0x3fffffff, 0x3ffffff0, 0x00000000], 3, 56, RM.TIES_AWAY, 0, 0, 0],
                [[0x3fffffff, 0x3ffffff8, 0x00000000], 3, [1, 0, 0], 3, 56, RM.NEAREST, 0, 0, 1],
                [[0x3fffffff, 0x3ffffff8, 0x00000000], 3, [0x3fffffff, 0x3ffffff0, 0x00000000], 3, 56, RM.TIES_ODD, 0, 0, 0],
                [[0x3fffffff, 0x3ffffff8, 0x00000001], 3, [1, 0, 0], 3, 56, RM.TIES_ODD, 0, 0, 1],
                [[0x3fffffff, 0x3ffffff8, 0x00000000], 3, [1, 0, 0], 3, 56, RM.TIES_ODD, 1, 0, 1],
                [[0x3fffffff, 0x3ffffff0, 0x00000000], 3, [0x3fffffff, 0x3ffffff0, 0x00000000], 3, 56, RM.UP, 0, 0, 0],
                [[0x3fffffff, 0x3ffffff0, 0x00000000], 3, [1, 0, 0], 3, 56, RM.UP, 1, 0, 1],
                [[0x3fffffff, 0x3ffffff0, 0x00000000], 3, [0x3fffffff, 0x3ffffff0, 0x00000000], 3, 56, RM.DOWN, 0, 0, 0],
                [[0x3fffffff, 0x3ffffff0, 0x00000000], 3, [0x3fffffff, 0x3ffffff0, 0x00000000], 3, 56, RM.DOWN, 1, 0, 0],
                [[0x3fffffff, 0x3ffffff0, 0x00000000], 3, [0x3fffffff, 0x3ffffff0, 0x00000000], 3, 56, RM.DOWN, 1, 0, 0],

                // Rounding occurs at a word boundary
                [[0x008954e2, 0x3089d80e, 0x00000000], 3, [0x008954e3, 0x00000000], 2, 24, RM.NEAREST, 0, 0, 0],
                [[0x008954e2, 0x3089d80e, 0x00000000], 3, [0x008954e2, 0x00000000], 2, 24, RM.DOWN, 0, 0, 0],
                [[0x008954e2, 0x3089d80e, 0x00000000], 3, [0x008954e3, 0x00000000], 2, 24, RM.UP, 0, 0, 0],
            ]

            for (let c of testCases) {
                testMantissaCase(roundMantissaToPrecision, c, args, c[2], c[8])
            }
        })
    })

    describe("addMantissas", () => {
        it("behaves like the reference implementation", () => {
            let argNames = [
                "m1", "m1Len", "m2", "m2Len", "m2shift", "t", "tLen", "prec", "rm"
            ]

            let cases = 0
            let startTime = Date.now()

            let mantissas = [...difficultMantissas, ...typicalMantissas]

            // About 102 million test cases, should be somewhat thorough in terms of carry and rounding checking
            for (let i = 0; i < mantissas.length; ++i) {
                const m1 = mantissas[i]
                for (const m2 of mantissas) {
                    for (let shift = 0; shift < 5; ++shift) {
                        for (let targetSize = 1; targetSize < 5; ++targetSize) {
                            for (let precision of [30, 53, 59, 60, 120]) {
                                for (let roundingMode of TEST_RM) {
                                    let target = new Int32Array(Math.max(neededWordsForPrecision(precision), targetSize))
                                    const args = [m1, m1.length, m2, m2.length, shift, target, target.length, precision, roundingMode]
                                    let ret = referenceAddMantissas(...args)

                                    testMantissaCase(addMantissas, args, argNames, target, ret)
                                    cases++
                                }
                            }
                        }
                    }
                }

                (!(i % 10)) ? console.log(`Progress: ${(i / mantissas.length * 100).toPrecision(4)}% complete`) : 0
            }

            let endTime = Date.now()
            let elapsed = (endTime - startTime) / 1000

            console.log(`Completed ${cases} test cases for addMantissas, comparing to referenceAddMantissas, in ${elapsed} seconds.`)
        })
    })

    describe("subtractMantissas", () => {
        it("behaves like the reference implementation", () => {
            let argNames = ["m1", "m1Len", "m2", "m2Len", "m2shift", "t", "tLen", "prec", "rm"]  // reference args, not actual

            let cases = 0
            let startTime = Date.now()

            // About 102 million test cases, should be thorough in terms of carry and rounding checking
            for (let i = 0; i < difficultMantissas.length; ++i) {
                const m1 = difficultMantissas[i]
                for (const m2 of difficultMantissas) {
                    for (let shift = 0; shift < 5; ++shift) {
                        // Eliminate invalid cases

                        for (let targetSize = 1; targetSize < 5; ++targetSize) {
                            for (let precision of [30, 53, 59, 60, 120]) {
                                for (let roundingMode of TEST_RM) {
                                    let target = new Int32Array(Math.max(neededWordsForPrecision(precision), targetSize))
                                    let ret = referenceSubtractMantissas(m1, m2, shift, precision, target, roundingMode)

                                    let noCheck = shift === 0 && (compareMantissas(m1, m1.length, m2, m2.length) === 0)
                                    //console.log(shift, m1, m2, ret, target, noCheck)

                                    testMantissaCase(subtractMantissas, [m1, m1.length, m2, m2.length, shift, target, target.length, precision, roundingMode], argNames, target, ret, noCheck)
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
            let argNames = ["m1", "m1Len", "m2", "m2Len", "t", "tLen", "prec", "rm"]

            let cases = 0
            let startTime = Date.now()

            let testMantissas = [ ...typicalMantissas, ...difficultMantissas ]

            for (let i = 0; i < testMantissas.length; ++i) {
                const m1 = testMantissas[i]
                for (const m2 of testMantissas) {
                    for (let targetSize = 0; targetSize < 5; ++targetSize) {
                        for (let precision of [30, 53, 59, 60, 120]) {
                            for (let roundingMode of TEST_RM) {
                                let target = new Int32Array(neededWordsForPrecision(precision))
                                let ret = referenceMultiplyMantissas(m1, m2, precision, target, roundingMode)

                                testMantissaCase(multiplyMantissas, [m1, m1.length, m2, m2.length, target, target.length, precision, roundingMode], argNames, target, ret)
                                cases++
                            }
                        }
                    }
                }

                (!(i % 10)) ? console.log(`Progress: ${(i / testMantissas.length * 100).toPrecision(4)}% complete`) : 0
            }

            let endTime = Date.now()
            console.log(`Completed ${cases} test cases for multiplyMantissas, comparing to referenceMultiplyMantissas, in ${(endTime - startTime) / 1000} seconds.`)
        })
    })

    describe("BigFloat.addTo", () => {
        it("Should work for f64", () => {
            expectMultipleCases((a, b) => {

                return BigFloat.add(BigFloat.fromNumber(a), BigFloat.fromNumber(b)).toNumber()
            }, cartesianProduct(ALL_NUMBERS, ALL_NUMBERS).map(([a, b]) => {
                return [[a, b], a + b]
            }))
        })

        it("Should work for f32", () => {
            expectMultipleCases((a, b) => {
                return BigFloat.add(BigFloat.fromNumber(a, 24), BigFloat.fromNumber(b, 24)).toNumber(ROUNDING_MODE.NEAREST, true)
            }, cartesianProduct(ALL_NUMBERS, ALL_NUMBERS).map(([a, b]) => {
                a = Math.fround(a)
                b = Math.fround(b)
                return [[a, b], Math.fround(a + b)]
            }))
        })

        it("Should work for whatever rounding", () => {
            expectMultipleCases((an, bn) => {
                let a = BigFloat.fromNumber(an)
                let b = BigFloat.fromNumber(bn)
                let prec = 53

                let precise = BigFloat.add(a, b, 1000, RM.NEAREST)
                let whatever = BigFloat.add(a, b, prec, RM.WHATEVER)

                let err = BigFloat.ulpError(precise, whatever)

                if (Math.abs(err) <= 1 || err !== err) return true
                return false
            }, (function* () {
                let g = cartesianProductGen(RANDOM_BIGFLOATS, RANDOM_BIGFLOATS)
                    for (let k of g) {
                        yield [k, true]
                    }
                })())
        })
    })

    describe("BigFloat.mulTo", () => {
        it("Should work for f64", () => {
            expectMultipleCases((a, b) => {

                return BigFloat.mul(BigFloat.fromNumber(a), BigFloat.fromNumber(b)).toNumber()
            }, cartesianProduct(ALL_NUMBERS, ALL_NUMBERS).map(([a, b]) => {
                return [[a, b], a * b]
            }))
        })

        it("Should work for f32", () => {
            expectMultipleCases((a, b) => {
                return BigFloat.mul(BigFloat.fromNumber(a, 24), BigFloat.fromNumber(b, 24)).toNumber(ROUNDING_MODE.NEAREST, true)
            }, cartesianProduct(ALL_NUMBERS, ALL_NUMBERS).map(([a, b]) => {
                a = Math.fround(a)
                b = Math.fround(b)
                return [[a, b], Math.fround(a * b)]
            }))
        })

        it("Should work for whatever rounding", () => {
            expectMultipleCases((an, bn) => {
                let a = BigFloat.fromNumber(an)
                let b = BigFloat.fromNumber(bn)
                let prec = 53

                let precise = BigFloat.mul(a, b, 1000, RM.NEAREST)
                let whatever = BigFloat.mul(a, b, prec, RM.WHATEVER)

                let err = BigFloat.ulpError(precise, whatever)

                if (Math.abs(err) <= 1 || err !== err) return true
                return false
            }, (function* () {
                let g = cartesianProductGen(RANDOM_BIGFLOATS, RANDOM_BIGFLOATS)
                for (let k of g) {
                    yield [k, true]
                }
            })())
        })
    })

    describe("BigFloat.ulpError", () => {
        it("Should give correct results", () => {
            expectMultipleCases((a, b) => {
                return BigFloat.ulpError(a, b)
            },
                [
                    [[1, 1], 0],
                    [[1, 1 + Number.EPSILON], 1],
                    [[1, 1 - Number.EPSILON / 2], -1],
                    [[-2, -2 - 2 * Number.EPSILON], -1],
                    [[2, 2 + 4 * Number.EPSILON], 2],
                    [[Infinity, Infinity], NaN],
                    [[-Infinity, Infinity], Infinity],
                    [[NaN, 4], NaN],
                    [[1042, 1042.000005], 21990233],
            ])
        })
    })

    describe("BigFloat.divTo", () => {
        it("Should work for f64", () => {

            expectMultipleCases((a, b) => {
                let prec = 53
                if (isDenormal(a / b)) {
                    prec = 100  // weird intermediate rounding effects
                }

                return BigFloat.div(BigFloat.fromNumber(a), BigFloat.fromNumber(b), prec).toNumber()
            }, cartesianProduct(ALL_NUMBERS, ALL_NUMBERS).map(([a, b]) => {
                return [[a, b], a / b]
            }))
        })

        it("Should work for f32", () => {
            expectMultipleCases((a, b) => {
                let prec = 24
                if (Math.abs(a / b) < 1.1754943508222875e-38) {
                    prec = 60  // weird intermediate rounding effects
                }

                return BigFloat.div(BigFloat.fromNumber(a, 24), BigFloat.fromNumber(b, 24), prec).toNumber(ROUNDING_MODE.NEAREST, true)
            }, cartesianProduct(ALL_NUMBERS.map(Math.fround), ALL_NUMBERS.map(Math.fround)).map(([a, b]) => {
                return [[a, b], Math.fround(a / b)]
            }))
        })

        it("Should work for bigints", () => {})
    })

})
