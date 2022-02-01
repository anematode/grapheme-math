import { expect } from "chai"
import {pedanticToString} from "./test_common.js"

function checkResult (f, args, res) {
  if (!Array.isArray(args)) args = [ args ]

  if (!Object.is(f(...args), res)) {
    // Converting to string helps with -0, etc since .to.equal is not Object.is
    expect(pedanticToString(f(...args))).to.equal(pedanticToString(res), "Input: " + args.join(', '))
  }
}

export function expectMultipleCases (f, inputs, name=f.name) {
  for (let input of inputs) {
    let args = input[0]
    let res = input[1]

    checkResult(f, args, res)
  }
}

export function expectAllEquals (f, inputs, res) {
  for (let args of inputs) {
    checkResult(f, args, res)
  }
}

export function expectEquivalentBehavior (test, reference, inputs) {
  let referenceResults = []

  for (let args of inputs) {
    if (!Array.isArray(args)) args = [ args ]
    referenceResults.push([ args, reference(...args) ])
  }

  expectMultipleCases(test, referenceResults)
}
