import { expect } from "chai"
import { describe, test } from "mocha"
import {isDenormal, pow2, roundDown, roundUp} from "../src/fp/manip.js"

function checkResult (f, args, res) {
  if (!Array.isArray(args)) args = [ args ]

  if (!Object.is(f(...args), res)) {
    // Converting to string helps with -0, etc since .to.equal is not Object.is
    expect(f(...args) + '').to.equal(res + '', "Input: " + args.join(', '))
  }
}

function expectMultipleCases (f, inputs, name=f.name) {
  for (let input of inputs) {
    let args = input[0]
    let res = input[1]

    checkResult(f, args, res)
  }
}

function expectAllEquals (f, inputs, res) {
  for (let args of inputs) {
    checkResult(f, args, res)
  }
}

function expectEquivalentBehavior (test, reference, inputs) {
  let referenceResults = []

  for (let args of inputs) {
    if (!Array.isArray(args)) args = [ args ]
    referenceResults.push([ args, reference(...args) ])
  }

  expectMultipleCases(test, referenceResults)
}

describe('roundUp', () => {
  test('roundUp correct on special float values', () => {
    const testCases = [
      [Infinity, Infinity],
      [-Infinity, -Number.MAX_VALUE],
      [NaN, NaN]
    ]

    expectMultipleCases(roundUp, testCases)
  })

  test('roundUp correct on normal floating point values', () => {
    const testCases = [
      [0, Number.MIN_VALUE],
      [-0, Number.MIN_VALUE],
      [1, 1 + Number.EPSILON],
      [-1, -0.9999999999999999],
      [Number.MAX_VALUE, Infinity],
      [599.9999999999999, 600],
      [-2048.0000000000005, -2048],
      [2.2250738585072014e-308, 2.225073858507202e-308],
      [-2.225073858507202e-308, -2.2250738585072014e-308]
    ]

    expectMultipleCases(roundUp, testCases)
  })

  test('roundUp correct on denormal numbers', () => {
    const testCases = [
      [-Number.MIN_VALUE, 0],
      [Number.MIN_VALUE, 9.881312917e-324],
      [-2.2250738585072014e-308, -2.225073858507201e-308],
      [2.225073858507201e-308, 2.2250738585072014e-308],
      [2.2250738585072004e-308, 2.225073858507201e-308]
    ]

    expectMultipleCases(roundUp, testCases)
  })
})

describe('roundDown', () => {
  test('roundDown correct on special float values', () => {
    const testCases = [
      [Infinity, Number.MAX_VALUE],
      [-Infinity, -Infinity],
      [NaN, NaN]
    ]

    expectMultipleCases(roundDown, testCases)
  })

  test('roundDown correct on normal floating point values', () => {
    const testCases = [
      [0, -Number.MIN_VALUE],
      [-0, -Number.MIN_VALUE],
      [1, 0.9999999999999999],
      [-1, -1 - Number.EPSILON],
      [-Number.MAX_VALUE, -Infinity],
      [600, 599.9999999999999],
      [-2047.9999999999998, -2048],
      [-2.225073858507201e-308, -2.2250738585072014e-308],
      [2.2250738585072014e-308, 2.225073858507201e-308]
    ]

    expectMultipleCases(roundDown, testCases)
  })

  test('roundDown correct on special float values', () => {
    const testCases = [
      [Number.MIN_VALUE, 0],
      [9.881312917e-324, Number.MIN_VALUE],
      [2.225073858507202e-308, 2.2250738585072014e-308],
      [-2.2250738585072014e-308, -2.225073858507202e-308]
    ]

    expectMultipleCases(roundDown, testCases)
  })

  test('roundDown correct on denormal numbers', () => {
    const testCases = [
      [Number.MIN_VALUE, 0],
      [9.881312917e-324, Number.MIN_VALUE],
      [-2.225073858507201e-308, -2.2250738585072014e-308],
      [2.225073858507201e-308, 2.2250738585072004e-308]
    ]

    expectMultipleCases(roundDown, testCases)
  })
})

test('isDenormal correct', () => {
  const fails = [
    0,
    -0,
    1,
    2.2250738585072014e-308,
    Infinity,
    NaN,
    -Infinity,
    -2.2250738585072014e-308
  ]
  const passes = [
    2.225073858507201e-308,
    Number.MIN_VALUE,
    -Number.MIN_VALUE,
    -2.225073858507201e-308
  ]

  expectAllEquals(isDenormal, fails, false)
  expectAllEquals(isDenormal, passes, true)
})

test('pow2 correct', () => {
  const testI = [Infinity, -Infinity, NaN]
  for (let i = -1080; i < 1080; i += 0.5) testI.push(i)

  expectEquivalentBehavior(pow2, n => 2 ** n, testI)
})
